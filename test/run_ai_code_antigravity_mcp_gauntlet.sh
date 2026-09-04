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
             "\\`\\(ai-code-test-antigravity-cli-start\\|ai-code-test-mcp-agent-\\(antigravity\\|enables-antigravity\\)\\|ai-code-test-mcp-http-server-scopes-modern-protocol-opt-out\\|test-ai-code-backends-infra-create-new-session-cleans-\\)")'
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
  local agents_before
  local agents_after
  local probe_output

  agents_before=$(git status --porcelain=v1 --untracked-files=all -- .agents)
  probe_output=$(emacs -Q --batch -L . -l test/test_00-bootstrap.el \
    -l ai-code-mcp-agent \
    --eval "(let* ((ai-code-mcp-agent-enabled-backends '(antigravity))
                    (ai-code-mcp-agent--antigravity-config-states
                     (make-hash-table :test 'equal))
                    (ai-code-mcp--sessions (make-hash-table :test 'equal))
                    (ai-code-mcp-http-server-port nil)
                    (ai-code-mcp-http-server--server nil)
                    (ai-code-mcp-http-server--port nil)
                    (output (generate-new-buffer
                             \" *ai-code-agy-gauntlet-probe*\"))
                    launch process session-id context sent)
               (unwind-protect
                   (progn
                     (setq launch
                           (ai-code-mcp-agent-prepare-launch
                            'antigravity default-directory '(\"agy\")))
                     (setq session-id (plist-get launch :mcp-session-id))
                     (setq process
                           (make-process
                            :name \"ai-code-agy-gauntlet-probe\"
                            :buffer output
                            :command '(\"agy\")
                            :connection-type 'pty
                            :noquery t))
                     (let ((started (float-time))
                           (deadline (+ (float-time) 20.0)))
                       (while (and
                               (process-live-p process)
                               (< (float-time) deadline)
                               (eq 'pending
                                   (plist-get
                                    (ai-code-mcp-get-session-context session-id)
                                    :state)))
                         (accept-process-output nil 0.1)
                         (when (and (not sent)
                                    (> (- (float-time) started) 3.0))
                           (process-send-string process \"/mcp\\r\")
                           (setq sent t))))
                     (setq context
                           (ai-code-mcp-get-session-context session-id))
                     (princ
                      (format \"state=%s protocol=%s client=%s\\n\"
                              (plist-get context :state)
                              (or (plist-get context :protocol-version)
                                  \"none\")
                              (or (alist-get
                                   'name (plist-get context :client-info))
                                  \"none\"))))
                 (when (process-live-p process)
                   (delete-process process))
                 (when-let ((release (plist-get launch :cleanup-fn)))
                   (funcall release))
                 (when (buffer-live-p output)
                   (kill-buffer output))))")
  printf '%s\n' "$probe_output"
  if [[ "$probe_output" != *"state=ready protocol=2025-11-25 client=antigravity-client"* ]]; then
    echo "Antigravity did not complete the legacy MCP handshake" >&2
    return 1
  fi
  agents_after=$(git status --porcelain=v1 --untracked-files=all -- .agents)
  if [[ "$agents_before" != "$agents_after" ]]; then
    echo "Antigravity probe changed workspace .agents state" >&2
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

emacs -Q --batch -L . -l ert \
  --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))" \
  -f ert-run-tests-batch-and-exit

run_byte_compile_gate "$task_tmp_dir"
run_checkdoc_gate
git diff --check
run_real_agy_probe

run_mutant "remove Antigravity from the default MCP backends" \
  "ai-code-mcp-agent.el" \
  's/\Qclaude-code antigravity)\E/claude-code)/'
run_mutant "omit the Bearer authorization scheme" \
  "ai-code-mcp-agent.el" \
  's/\Q(concat "Bearer " token)\E/token/'
run_mutant "discard the existing Antigravity JSON object" \
  "ai-code-mcp-agent.el" \
  's/\Q(if (plist-get state :original-exists-p)\E/(if nil/'
run_mutant "ignore the session modern-protocol opt-out" \
  "ai-code-mcp-http-server.el" \
  's/\Q(not (ai-code-mcp-http-server--modern-protocol-enabled-p context))\E/nil/'
run_mutant "skip cleanup on terminal startup failures" \
  "ai-code-backends-infra.el" \
  's/\Q(ai-code-backends-infra--cleanup-failed-launch cleanup-fn)\E/nil/g'

echo "Antigravity MCP gauntlet passed"
