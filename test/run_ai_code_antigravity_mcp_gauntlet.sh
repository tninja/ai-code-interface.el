#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
task_tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/ai-code-antigravity-mcp-gauntlet.XXXXXX")

cleanup() {
  case "$task_tmp_dir" in
    "${TMPDIR:-/tmp}"/ai-code-antigravity-mcp-gauntlet.*)
      rm -rf -- "$task_tmp_dir"
      ;;
  esac
}

trap cleanup EXIT

cd "$repo_root"

emacs --version | sed -n '1p'
git --version
perl -e 'printf "perl %vd\n", $^V'
agy --version

copy_task_files() {
  local destination=$1

  cp ai-code-antigravity-cli.el "$destination/ai-code-antigravity-cli.el"
  cp ai-code-backends-infra.el "$destination/ai-code-backends-infra.el"
  cp ai-code-mcp-agent.el "$destination/ai-code-mcp-agent.el"
  cp ai-code-mcp-http-server.el "$destination/ai-code-mcp-http-server.el"
  cp test/test_ai-code-antigravity-cli.el \
    "$destination/test_ai-code-antigravity-cli.el"
  cp test/test_ai-code-backends-infra.el \
    "$destination/test_ai-code-backends-infra.el"
  cp test/test_ai-code-mcp-agent.el "$destination/test_ai-code-mcp-agent.el"
  cp test/test_ai-code-mcp-http-server.el \
    "$destination/test_ai-code-mcp-http-server.el"
}

run_focused_tests() {
  local source_dir=$1
  local emacs_home="$task_tmp_dir/emacs.d/"

  emacs -Q --batch \
    --eval "(setq user-emacs-directory \"$emacs_home\" load-prefer-newer t)" \
    -L test/stubs -L "$source_dir" -L "$repo_root" \
    -l ert \
    -l "$source_dir/ai-code-backends-infra.el" \
    -l "$source_dir/ai-code-mcp-http-server.el" \
    -l "$source_dir/ai-code-mcp-agent.el" \
    -l "$source_dir/ai-code-antigravity-cli.el" \
    -l "$source_dir/test_ai-code-antigravity-cli.el" \
    -l "$source_dir/test_ai-code-backends-infra.el" \
    -l "$source_dir/test_ai-code-mcp-agent.el" \
    -l "$source_dir/test_ai-code-mcp-http-server.el" \
    --eval '(ert-run-tests-batch-and-exit
             "\\`\\(ai-code-test-antigravity-cli-start\\|ai-code-test-mcp-agent-\\(antigravity\\|enables-antigravity\\|kill-emacs-hook\\|skips-remote\\)\\|ai-code-test-mcp-http-server-scopes-modern-protocol-opt-out\\|test-ai-code-backends-infra-\\(create-new-session-cleans-\\|start-cli-session-\\(skips\\|runs\\)-prepare-launch\\)\\)")'
}

run_byte_compile_gate() {
  local source_dir=$1
  local compile_log="$task_tmp_dir/byte-compile.log"

  if ! emacs -Q --batch \
    --eval '(setq load-prefer-newer t)' \
    -L test/stubs -L "$source_dir" -L "$repo_root" \
    -f batch-byte-compile \
    "$source_dir/ai-code-antigravity-cli.el" \
    "$source_dir/ai-code-backends-infra.el" \
    "$source_dir/ai-code-mcp-agent.el" \
    "$source_dir/ai-code-mcp-http-server.el" \
    >"$compile_log" 2>&1; then
    cat "$compile_log" >&2
    return 1
  fi
  if rg -n 'Warning:|Error:' "$compile_log"; then
    echo "Byte compilation emitted diagnostics" >&2
    return 1
  fi
}

run_checkdoc_gate() {
  local checkdoc_log="$task_tmp_dir/checkdoc.log"

  if ! emacs -Q --batch -L test/stubs -L "$repo_root" -l checkdoc \
    --eval "(let ((checkdoc-force-docstrings-flag nil))
              (mapc #'checkdoc-file
                    '(\"ai-code-antigravity-cli.el\"
                      \"ai-code-backends-infra.el\"
                      \"ai-code-mcp-agent.el\"
                      \"ai-code-mcp-http-server.el\"
                      \"test/test_ai-code-antigravity-cli.el\"
                      \"test/test_ai-code-backends-infra.el\"
                      \"test/test_ai-code-mcp-agent.el\"
                      \"test/test_ai-code-mcp-http-server.el\")))" \
    >"$checkdoc_log" 2>&1; then
    cat "$checkdoc_log" >&2
    return 1
  fi
  if [[ -s "$checkdoc_log" ]]; then
    cat "$checkdoc_log" >&2
    echo "Checkdoc emitted diagnostics" >&2
    return 1
  fi
}

run_real_agy_probe() {
  local probe_output

  probe_output=$(emacs -Q --batch --eval '(setq load-prefer-newer t)' -L . -l test/test_00-bootstrap.el \
    -l ai-code-mcp-agent \
    --eval "(let* ((tmp-ws (make-temp-file \"ai-code-agy-gauntlet-ws-\" t))
                    (ai-code-mcp-agent-enabled-backends '(antigravity))
                    (ai-code-mcp-agent--active-cleanups
                     (make-hash-table :test 'equal))
                    (ai-code-mcp--sessions (make-hash-table :test 'equal))
                    (ai-code-mcp-http-server-port nil)
                    (ai-code-mcp-http-server--server nil)
                    (ai-code-mcp-http-server--port nil)
                    (output1 (generate-new-buffer
                              \" *ai-code-agy-gauntlet-probe-1*\"))
                    (output2 (generate-new-buffer
                              \" *ai-code-agy-gauntlet-probe-2*\"))
                    (seen (make-hash-table :test 'equal))
                    (orig-dispatch (symbol-function 'ai-code-mcp-dispatch))
                    launch1 launch2 proc1 proc2 sid1 sid2 dir1 dir2 sent)
               (unwind-protect
                   (cl-letf (((symbol-function 'ai-code-mcp-dispatch)
                              (lambda (method &optional params)
                                (let ((key (cons ai-code-mcp--current-session-id method)))
                                  (puthash key
                                           (or ai-code-mcp--current-protocol-version
                                               (plist-get (ai-code-mcp-get-session-context)
                                                          :protocol-version))
                                           seen))
                                (funcall orig-dispatch method params))))
                     (call-process \"git\" nil nil nil \"-C\" tmp-ws \"init\" \"-q\")
                     (let* ((default-directory (file-name-as-directory tmp-ws))
                            (settings (format \"{\\\"trustedWorkspaces\\\":[\\\"%s\\\"]}\" tmp-ws)))
                       (setq launch1
                             (ai-code-mcp-agent-prepare-launch
                              'antigravity tmp-ws (list \"agy\" \"--settings\" settings))
                             launch2
                             (ai-code-mcp-agent-prepare-launch
                              'antigravity tmp-ws (list \"agy\" \"--settings\" settings)))
                       (setq sid1 (plist-get launch1 :mcp-session-id)
                             sid2 (plist-get launch2 :mcp-session-id)
                             dir1 (cadr (member \"--add-dir\" (plist-get launch1 :argv)))
                             dir2 (cadr (member \"--add-dir\" (plist-get launch2 :argv))))
                       (funcall (plist-get launch1 :post-start-fn) output1 nil \"i1\")
                       (funcall (plist-get launch2 :post-start-fn) output2 nil \"i2\")
                       (setq proc1
                             (make-process
                              :name \"ai-code-agy-gauntlet-probe-1\"
                              :buffer output1
                              :command (plist-get launch1 :argv)
                              :connection-type 'pty
                              :noquery t)
                             proc2
                             (make-process
                              :name \"ai-code-agy-gauntlet-probe-2\"
                              :buffer output2
                              :command (plist-get launch2 :argv)
                              :connection-type 'pty
                              :noquery t))
                       (let ((started (float-time))
                             (deadline (+ (float-time) 20.0)))
                         (while (and
                                 (or (process-live-p proc1) (process-live-p proc2))
                                 (< (float-time) deadline)
                                 (not (and (gethash (cons sid1 \"tools/list\") seen)
                                           (gethash (cons sid2 \"tools/list\") seen))))
                           (accept-process-output nil 0.1)
                           (when (and (not sent)
                                      (> (- (float-time) started) 3.0))
                             (dolist (p (list proc1 proc2))
                               (when (process-live-p p)
                                 (process-send-string p \"/mcp\\r\")))
                             (setq sent t))))
                       (dolist (pair (list (cons \"1\" sid1) (cons \"2\" sid2)))
                         (let* ((sid (cdr pair))
                                (context (ai-code-mcp-get-session-context sid)))
                           (princ
                            (format \"instance=%s state=%s discover=%s tools_list=%s\\n\"
                                    (car pair)
                                    (plist-get context :state)
                                    (or (gethash (cons sid \"server/discover\") seen) \"none\")
                                    (or (gethash (cons sid \"tools/list\") seen) \"none\")))))
                       (princ (format \"ws_agents_exists=%s\\n\"
                                      (file-exists-p
                                       (expand-file-name \".agents\" tmp-ws))))))
                 (dolist (p (list proc1 proc2))
                   (when (and p (process-live-p p))
                     (delete-process p)))
                 (dolist (l (list launch1 launch2))
                   (when-let ((release (plist-get l :cleanup-fn)))
                     (funcall release)))
                 (dolist (b (list output1 output2))
                   (when (buffer-live-p b)
                     (kill-buffer b)))
                 (princ (format \"temp_dirs_cleaned=%s\\n\"
                                (and (not (file-exists-p dir1))
                                     (not (file-exists-p dir2)))))
                 (when (file-directory-p tmp-ws)
                   (delete-directory tmp-ws t)))))")
  printf '%s\n' "$probe_output"
  if [[ "$probe_output" != *"instance=1 state=ready discover=2026-07-28 tools_list=2026-07-28"* ]] || \
     [[ "$probe_output" != *"instance=2 state=ready discover=2026-07-28 tools_list=2026-07-28"* ]] || \
     [[ "$probe_output" != *"ws_agents_exists=nil"* ]] || \
     [[ "$probe_output" != *"temp_dirs_cleaned=t"* ]]; then
    echo "Concurrent Antigravity instances did not complete isolated modern MCP handshakes" >&2
    return 1
  fi
}

run_mutant() {
  local description=$1
  local target_file=$2
  local expression=$3
  local mutant_dir="$task_tmp_dir/mutant"
  local mutant_log="$task_tmp_dir/mutant.log"

  rm -rf -- "$mutant_dir"
  mkdir -p "$mutant_dir"
  copy_task_files "$mutant_dir"
  perl -0pi -e "$expression" "$mutant_dir/$target_file"

  if cmp -s "$target_file" "$mutant_dir/$target_file"; then
    echo "Mutation did not change source: $description" >&2
    return 1
  fi
  if run_focused_tests "$mutant_dir" >"$mutant_log" 2>&1; then
    echo "Mutation survived: $description" >&2
    return 1
  fi
  if ! rg -q 'FAILED' "$mutant_log" || \
     rg -q 'Cannot open load file|invalid-read-syntax|End of file during parsing' \
       "$mutant_log"; then
    cat "$mutant_log" >&2
    echo "Mutation did not reach an assertion failure: $description" >&2
    return 1
  fi
  echo "Mutation killed: $description"
}

copy_task_files "$task_tmp_dir"

run_focused_tests "$task_tmp_dir"

emacs -Q --batch --eval '(setq load-prefer-newer t)' -L . -l ert \
  --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))" \
  -f ert-run-tests-batch-and-exit

run_byte_compile_gate "$task_tmp_dir"
run_checkdoc_gate
git diff --check
run_real_agy_probe

run_mutant "remove Antigravity from the default MCP backends" \
  "ai-code-mcp-agent.el" \
  's/\Qclaude-code antigravity pi)\E/claude-code pi)/'
run_mutant "omit the Bearer authorization scheme" \
  "ai-code-mcp-agent.el" \
  's/\Q(concat "Bearer " token)\E/token/'
run_mutant "omit the --add-dir flag for Antigravity" \
  "ai-code-mcp-agent.el" \
  's/\Q(append argv (list "--add-dir" config-dir))\E/argv/'
run_mutant "ignore the session modern-protocol opt-out" \
  "ai-code-mcp-http-server.el" \
  's/\Q(not (ai-code-mcp-http-server--modern-protocol-enabled-p context))\E/nil/'
run_mutant "skip cleanup on terminal startup failures" \
  "ai-code-backends-infra.el" \
  's/\Q(ai-code-backends-infra--cleanup-failed-launch cleanup-fn)\E/nil/g'

echo "Antigravity MCP gauntlet passed"
