#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
task_tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/ai-code-issue-478-gauntlet.XXXXXX")

cleanup() {
  case "$task_tmp_dir" in
    "${TMPDIR:-/tmp}"/ai-code-issue-478-gauntlet.*)
      rm -rf -- "$task_tmp_dir"
      ;;
  esac
}

trap cleanup EXIT

cd "$repo_root"

emacs --version | sed -n '1p'
git --version
perl -e 'printf "perl %vd\n", $^V'

copy_issue_files() {
  local destination=$1

  cp ai-code-harness.el "$destination/ai-code-harness.el"
  cp ai-code-prompt-mode.el "$destination/ai-code-prompt-mode.el"
  cp test/test_ai-code-harness.el "$destination/test_ai-code-harness.el"
  cp test/test_ai-code-prompt-mode.el "$destination/test_ai-code-prompt-mode.el"
}

run_focused_tests() {
  local source_dir=$1
  local emacs_home="$task_tmp_dir/emacs.d/"

  emacs -Q --batch \
    --eval "(setq user-emacs-directory \"$emacs_home\" load-prefer-newer t)" \
    -L test/stubs -L "$source_dir" -L "$repo_root" \
    -l ert \
    -l "$source_dir/ai-code-harness.el" \
    -l "$source_dir/test_ai-code-harness.el" \
    --eval '(ert-run-tests-batch-and-exit
             "ai-code-test-\\(github-analysis-workflows-bypass-auto-test-routing\\|resolve-auto-test-type-for-send-\\(question-skips-when-gptel-disabled\\|unknown-asks-without-gptel\\)\\|simple-classifier-treats-todo-question-brief-as-non-code-change\\)")' \
    || return 1

  emacs -Q --batch \
    --eval "(setq user-emacs-directory \"$emacs_home\" load-prefer-newer t)" \
    -L test/stubs -L "$source_dir" -L "$repo_root" \
    -l ert \
    -l "$source_dir/ai-code-prompt-mode.el" \
    -l "$source_dir/test_ai-code-prompt-mode.el" \
    --eval '(ert-run-tests-batch-and-exit
             "ai-code-test-call-gptel-sync-\\(ignores-reasoning-before-final-response\\|reports-gptel-error-field\\)")' \
    || return 1
}

run_mutant() {
  local description=$1
  local target_file=$2
  local expression=$3
  local mutant_dir="$task_tmp_dir/mutant"

  rm -rf -- "$mutant_dir"
  mkdir -p "$mutant_dir"
  copy_issue_files "$mutant_dir"
  perl -0pi -e "$expression" "$mutant_dir/$target_file"

  if cmp -s "$target_file" "$mutant_dir/$target_file"; then
    echo "Mutation did not change source: $description" >&2
    exit 1
  fi

  if run_focused_tests "$mutant_dir" >/dev/null 2>&1; then
    echo "Mutation survived: $description" >&2
    exit 1
  fi
  echo "Mutation killed: $description"
}

copy_issue_files "$task_tmp_dir"

run_focused_tests "$task_tmp_dir"

emacs -Q --batch --eval '(setq load-prefer-newer t)' -L . -l ert \
  --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))" \
  -f ert-run-tests-batch-and-exit

emacs -Q --batch \
  --eval "(setq user-emacs-directory \"$task_tmp_dir/emacs.d/\" load-prefer-newer t)" \
  -L test/stubs -L "$task_tmp_dir" -L . \
  -f batch-byte-compile \
  "$task_tmp_dir/ai-code-prompt-mode.el" \
  "$task_tmp_dir/ai-code-harness.el"

emacs -Q --batch -L test/stubs -L . -l checkdoc \
  --eval '(progn (checkdoc-file "ai-code-harness.el")
                 (checkdoc-file "ai-code-prompt-mode.el")
                 (checkdoc-file "test/test_ai-code-harness.el")
                 (checkdoc-file "test/test_ai-code-prompt-mode.el"))'

git diff --check

run_mutant "call GPTel when local fallback is disabled" \
  "ai-code-harness.el" \
  's/\Qai-code-use-gptel-classify-prompt)\E/t)/'
run_mutant "drop TODO question markers" \
  "ai-code-harness.el" \
  's/ai-code-change--ask-question-note\s+ai-code-change--question-brief-default-boundaries\s+//'
run_mutant "ignore GitHub workflow metadata" \
  "ai-code-harness.el" \
  's/\Qai-code--non-code-change-workflows)\E/nil)/'
run_mutant "treat reasoning as an unknown response" \
  "ai-code-prompt-mode.el" \
  "s/'reasoning/'never-reasoning/"
run_mutant "ignore GPTel callback errors" \
  "ai-code-prompt-mode.el" \
  's/\Q(plist-get info :error)\E/nil/'

echo "Issue 478 gauntlet passed"
