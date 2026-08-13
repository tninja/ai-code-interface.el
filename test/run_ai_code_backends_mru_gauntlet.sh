#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
task_tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/ai-code-mru-gauntlet.XXXXXX")

cd "$repo_root"

emacs --version | sed -n '1p'
git --version
perl -e 'printf "perl %vd\n", $^V'

run_focused_tests() {
  local source_dir=$1
  emacs -Q --batch \
    --eval "(setq user-emacs-directory \"$task_tmp_dir/emacs.d/\" load-prefer-newer t)" \
    -L "$source_dir" -L "$repo_root" \
    -l ert -l "$source_dir/test_ai-code-backends.el" \
    --eval '(ert-run-tests-batch-and-exit "^test-ai-code-backends--")'
}

run_mutant() {
  local description=$1
  local expression=$2
  local mutant_dir="$task_tmp_dir/mutant"

  mkdir -p "$mutant_dir"
  cp ai-code-backends.el "$mutant_dir/ai-code-backends.el"
  cp test/test_ai-code-backends.el "$mutant_dir/test_ai-code-backends.el"
  perl -0pi -e "$expression" "$mutant_dir/ai-code-backends.el"

  if run_focused_tests "$mutant_dir" >/dev/null 2>&1; then
    echo "Mutation survived: $description" >&2
    exit 1
  fi
  echo "Mutation killed: $description"
}

cp ai-code-backends.el "$task_tmp_dir/ai-code-backends.el"
cp test/test_ai-code-backends.el "$task_tmp_dir/test_ai-code-backends.el"

run_focused_tests "$task_tmp_dir"

emacs -Q --batch --eval '(setq load-prefer-newer t)' -L . -l ert \
  --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))" \
  -f ert-run-tests-batch-and-exit

emacs -Q --batch \
  --eval "(setq user-emacs-directory \"$task_tmp_dir/emacs.d/\" load-prefer-newer t)" \
  -L test/stubs -L . -L "$task_tmp_dir" \
  -f batch-byte-compile "$task_tmp_dir/ai-code-backends.el" \
  "$task_tmp_dir/test_ai-code-backends.el"

emacs -Q --batch -L . -l checkdoc \
  --eval '(progn (checkdoc-file "ai-code-backends.el")
                 (checkdoc-file "test/test_ai-code-backends.el"))'

git diff --check

run_mutant "accept trailing history data" \
  's/\(eobp\)/t/'
run_mutant "retain duplicate history entries" \
  's/\(delete-dups history\)/history/'
run_mutant "sort completion metadata in reverse" \
  's/\(display-sort-function \. identity\)/(display-sort-function . reverse)/'
run_mutant "allow Helm fuzzy sorting" \
  's/\(ai-code-select-backend \. emacs\)/(ai-code-select-backend . helm-fuzzy)/'
run_mutant "allow Ivy alphabetical sorting" \
  's/\(ai-code-select-backend \. nil\)/(ai-code-select-backend . string-lessp)/'

echo "MRU gauntlet passed; temporary evidence is in $task_tmp_dir"
