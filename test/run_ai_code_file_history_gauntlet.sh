#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
task_tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/ai-code-file-history-gauntlet.XXXXXX")

cleanup() {
  case "$task_tmp_dir" in
    "${TMPDIR:-/tmp}"/ai-code-file-history-gauntlet.*)
      rm -rf -- "$task_tmp_dir"
      ;;
  esac
}

trap cleanup EXIT

cd "$repo_root"

emacs --version | sed -n '1p'
git --version
perl -e 'printf "perl %vd\n", $^V'

run_focused_tests() {
  local source_dir=$1

  emacs -Q --batch \
    --eval "(setq user-emacs-directory \"$task_tmp_dir/emacs.d/\" load-prefer-newer t)" \
    -L "$source_dir" -L "$repo_root" \
    -l ert -l "$source_dir/test_ai-code-file.el" \
    --eval '(ert-run-tests-batch-and-exit "^ai-code-test-run-current-file-or-shell-cmd-")'
}

run_mutant() {
  local description=$1
  local expression=$2
  local mutant_dir="$task_tmp_dir/mutant"

  rm -rf -- "$mutant_dir"
  mkdir -p "$mutant_dir"
  cp ai-code-file.el "$mutant_dir/ai-code-file.el"
  cp test/test_ai-code-file.el "$mutant_dir/test_ai-code-file.el"
  perl -0pi -e "$expression" "$mutant_dir/ai-code-file.el"

  if run_focused_tests "$mutant_dir" >/dev/null 2>&1; then
    echo "Mutation survived: $description" >&2
    exit 1
  fi
  echo "Mutation killed: $description"
}

cp ai-code-file.el "$task_tmp_dir/ai-code-file.el"
cp test/test_ai-code-file.el "$task_tmp_dir/test_ai-code-file.el"

run_focused_tests "$task_tmp_dir"

emacs -Q --batch --eval '(setq load-prefer-newer t)' -L . -l ert \
  --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))" \
  -f ert-run-tests-batch-and-exit

emacs -Q --batch \
  --eval "(setq user-emacs-directory \"$task_tmp_dir/emacs.d/\" load-prefer-newer t)" \
  -L test/stubs -L . -L "$task_tmp_dir" \
  -f batch-byte-compile "$task_tmp_dir/ai-code-file.el"

emacs -Q --batch -L . -l checkdoc \
  --eval '(progn (checkdoc-file "ai-code-file.el")
                 (checkdoc-file "test/test_ai-code-file.el"))'

git diff --check

run_mutant "remove dedicated history fallback" \
  's/\(or initial-input\s+\(car ai-code-shell-command-history\)\)/initial-input/'
run_mutant "prefer history over explicit region input" \
  's/\(or initial-input\s+\(car ai-code-shell-command-history\)\)/(or (car ai-code-shell-command-history) initial-input)/'
run_mutant "skip confirmed AI command history" \
  "s/\\(add-to-history 'ai-code-shell-command-history/(ignore 'ai-code-shell-command-history/"

echo "AI Code file history gauntlet passed"
