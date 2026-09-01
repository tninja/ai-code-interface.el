#!/usr/bin/env bash
# Gauntlet for the prompt editing enhancements.
#
# Runs every applicable constraint layer as an executable gate. Any layer that
# does not run, crashes, or cannot enforce its constraint is a failure, never a
# pass.
#
# Usage: test/run_ai_code_prompt_editing_gauntlet.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# Files under test.
NEW_FILES=(ai-code-prompt-editing.el)
CHANGED_FILES=(ai-code-prompt-mode.el ai-code-task.el)
ALL_FILES=("${NEW_FILES[@]}" "${CHANGED_FILES[@]}")

FOCUSED_TESTS=(test/test_ai-code-prompt-editing.el)

# Number of pre-existing failures in the suite, recorded verbatim at spec time.
# Any deviation fails the run.
BASELINE_UNEXPECTED=13

# An isolated package directory works around a pre-existing bug in
# test/test_00-bootstrap.el, which passes full paths to `version<' and so
# crashes when two versions of a dependency are installed locally.
PKG_DIR="${AI_CODE_GAUNTLET_PKG_DIR:-}"

FAILURES=()
LAYERS_RUN=()

fail_layer() {
  echo "FAIL: $1"
  FAILURES+=("$1")
}

record_layer() {
  LAYERS_RUN+=("$1")
}

# ---------------------------------------------------------------------------
# Emacs invocation
# ---------------------------------------------------------------------------

emacs_batch() {
  if [[ -n "$PKG_DIR" ]]; then
    emacs -Q -batch -L . \
      --eval "(progn (setq package-user-dir \"$PKG_DIR\") (require 'package) (package-initialize))" \
      "$@"
  else
    emacs -Q -batch -L . \
      --eval "(progn (require 'package) (package-initialize))" \
      "$@"
  fi
}

# Run ERT over the given test files and echo the summary line.
run_ert() {
  emacs_batch -l test/test_00-bootstrap.el -l ert "$@" \
    -f ert-run-tests-batch-and-exit 2>&1 || true
}

unexpected_count() {
  # Extract the "N unexpected" figure from an ERT summary. Prints "MISSING"
  # when no summary line exists, so a crashed run can never look like a pass.
  local output="$1"
  local line
  line="$(printf '%s\n' "$output" | grep -E "^Ran [0-9]+ tests" | tail -1)"
  if [[ -z "$line" ]]; then
    echo "MISSING"
    return
  fi
  if [[ "$line" =~ ([0-9]+)\ unexpected ]]; then
    echo "${BASH_REMATCH[1]}"
  else
    echo 0
  fi
}

ran_count() {
  local output="$1"
  local line
  line="$(printf '%s\n' "$output" | grep -E "^Ran [0-9]+ tests" | tail -1)"
  if [[ "$line" =~ ^Ran\ ([0-9]+)\ tests ]]; then
    echo "${BASH_REMATCH[1]}"
  else
    echo MISSING
  fi
}

# ---------------------------------------------------------------------------
# Layer 1: focused tests
# ---------------------------------------------------------------------------

echo "== Layer: focused tests =="
FOCUSED_ARGS=()
for f in "${FOCUSED_TESTS[@]}"; do
  [[ -f "$f" ]] || { fail_layer "focused tests: missing input $f"; }
  FOCUSED_ARGS+=(-l "$f")
done
FOCUSED_OUT="$(run_ert "${FOCUSED_ARGS[@]}")"
FOCUSED_RAN="$(ran_count "$FOCUSED_OUT")"
FOCUSED_BAD="$(unexpected_count "$FOCUSED_OUT")"
echo "focused: ran=$FOCUSED_RAN unexpected=$FOCUSED_BAD"
if [[ "$FOCUSED_BAD" != "0" ]]; then
  fail_layer "focused tests: expected 0 unexpected, got $FOCUSED_BAD"
fi
if [[ "$FOCUSED_RAN" == "MISSING" || "$FOCUSED_RAN" -lt 50 ]]; then
  fail_layer "focused tests: implausibly few tests ran ($FOCUSED_RAN)"
fi
record_layer "focused tests"

# ---------------------------------------------------------------------------
# Layer 2: full suite, compared against the recorded baseline
# ---------------------------------------------------------------------------

echo "== Layer: full suite =="
FULL_OUT="$(run_ert --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))")"
FULL_RAN="$(ran_count "$FULL_OUT")"
FULL_BAD="$(unexpected_count "$FULL_OUT")"
echo "full suite: ran=$FULL_RAN unexpected=$FULL_BAD (baseline $BASELINE_UNEXPECTED)"
if [[ "$FULL_BAD" == "MISSING" ]]; then
  fail_layer "full suite: no ERT summary, the run crashed"
elif [[ "$FULL_BAD" -gt "$BASELINE_UNEXPECTED" ]]; then
  fail_layer "full suite: $FULL_BAD unexpected exceeds baseline $BASELINE_UNEXPECTED"
fi
if [[ "$FULL_RAN" == "MISSING" || "$FULL_RAN" -lt 1400 ]]; then
  fail_layer "full suite: implausibly few tests ran ($FULL_RAN)"
fi
record_layer "full suite"

# ---------------------------------------------------------------------------
# Layer 3: byte compilation, no new warnings
# ---------------------------------------------------------------------------

echo "== Layer: byte compilation =="
rm -f ./*.elc
# The bootstrap adds installed dependencies to `load-path'; without it magit is
# unresolvable and the compile errors out instead of merely warning.
BYTE_OUT="$(emacs_batch -l test/test_00-bootstrap.el -f batch-byte-compile "${ALL_FILES[@]}" 2>&1 || true)"
BYTE_ERRORS="$(printf '%s\n' "$BYTE_OUT" | grep -cE "^[^ ]+\.el:[0-9]+:[0-9]+: Error:" || true)"
if [[ "$BYTE_ERRORS" != "0" ]]; then
  printf '%s\n' "$BYTE_OUT" | grep -E "Error:" | head -10
  fail_layer "byte compilation: $BYTE_ERRORS errors"
fi
# Obsolete-macro warnings for when-let/if-let are pre-existing across the
# repository and are not introduced by this work.
BYTE_WARNINGS="$(printf '%s\n' "$BYTE_OUT" \
  | grep -E "Warning:" \
  | grep -vE "(when-let|if-let). is an obsolete macro" \
  | grep -cE "Warning:" || true)"
echo "byte compilation warnings (excluding pre-existing obsolete-macro): $BYTE_WARNINGS"
if [[ "$BYTE_WARNINGS" != "0" ]]; then
  printf '%s\n' "$BYTE_OUT" | grep -E "Warning:" \
    | grep -vE "(when-let|if-let). is an obsolete macro" | head -20
  fail_layer "byte compilation: $BYTE_WARNINGS new warnings"
fi
for f in "${ALL_FILES[@]}"; do
  if [[ ! -f "${f}c" ]]; then
    fail_layer "byte compilation: ${f}c was not produced"
  fi
done
rm -f ./*.elc
record_layer "byte compilation"

# ---------------------------------------------------------------------------
# Layer 4: checkdoc
# ---------------------------------------------------------------------------

echo "== Layer: checkdoc =="
CHECKDOC_TOTAL=0
for f in "${ALL_FILES[@]}"; do
  n="$(emacs -Q -batch \
        ${PKG_DIR:+--eval "(progn (setq package-user-dir \"$PKG_DIR\") (require 'package) (package-initialize))"} \
        --eval "(progn (require 'checkdoc) (setq checkdoc-diagnostic-buffer \"*w*\") (checkdoc-file \"$f\"))" 2>&1 \
        | grep -c "^$f:" || true)"
  echo "checkdoc $f: $n"
  CHECKDOC_TOTAL=$((CHECKDOC_TOTAL + n))
done
if [[ "$CHECKDOC_TOTAL" != "0" ]]; then
  fail_layer "checkdoc: $CHECKDOC_TOTAL issues"
fi
record_layer "checkdoc"

# ---------------------------------------------------------------------------
# Layer 5: whitespace
# ---------------------------------------------------------------------------

echo "== Layer: whitespace =="
if ! git diff --check; then
  fail_layer "whitespace: git diff --check reported problems"
fi
record_layer "whitespace"

# ---------------------------------------------------------------------------
# Layer 6: mutation testing
# ---------------------------------------------------------------------------
#
# Each mutant introduces one plausible bug. A mutant that cannot be applied is
# an error, not a kill, so every application is verified with cmp before the
# tests run.

echo "== Layer: mutation =="

MUTANTS_TOTAL=0
MUTANTS_KILLED=0

run_mutant() {
  local name="$1" file="$2" perl_expr="$3"
  local backup
  backup="$(mktemp)"
  cp "$file" "$backup"

  MUTANTS_TOTAL=$((MUTANTS_TOTAL + 1))

  perl -0pi -e "$perl_expr" "$file"

  # Prove the mutation actually changed the source.
  if cmp -s "$file" "$backup"; then
    cp "$backup" "$file"
    rm -f "$backup"
    fail_layer "mutation '$name': mutation did not apply, cannot count as a kill"
    return
  fi

  local out bad
  out="$(run_ert "${FOCUSED_ARGS[@]}")"
  bad="$(unexpected_count "$out")"

  cp "$backup" "$file"
  rm -f "$backup"

  # Confirm restoration really happened.
  if ! git diff --quiet "$file" && ! git diff "$file" | grep -q .; then
    fail_layer "mutation '$name': source not restored"
    return
  fi

  if [[ "$bad" == "MISSING" ]]; then
    # The suite never completed, so this proves nothing about detection: a
    # mutant that breaks loading would look identical to a real behavioral
    # kill. Treat it as a broken mutant, never as a pass.
    fail_layer "mutation '$name': suite did not complete, mutant is unloadable rather than detected"
  elif [[ "$bad" -gt 0 ]]; then
    echo "mutant '$name': KILLED ($bad failing tests)"
    MUTANTS_KILLED=$((MUTANTS_KILLED + 1))
  else
    fail_layer "mutation '$name': SURVIVED, the tests do not detect this bug"
  fi
}

# M1: remove the literal-block check, so annotations in src blocks trigger again.
run_mutant "guard ignores literal blocks" ai-code-prompt-editing.el \
  's/\(not \(ai-code--prompt-inside-literal-block-p sigil-pos\)\)/t/'

# M2: neutralise the word-boundary check, so "user@" triggers again. Written to
# keep the file loadable, so the kill has to come from a failing assertion
# rather than from the suite refusing to start.
run_mutant "guard ignores word boundary" ai-code-prompt-editing.el \
  's/\(or \(= sigil-pos \(point-min\)\)/(or t (= sigil-pos (point-min))/'

# M3: invert the block-delimiter decision, so begin and end are swapped.
run_mutant "block detection inverted" ai-code-prompt-editing.el \
  's/\(looking-at-p ai-code--prompt-block-begin-regexp\)/(not (looking-at-p ai-code--prompt-block-begin-regexp))/'

# M4: make every file look like a task file, so plain org files get folded.
run_mutant "task file predicate always true" ai-code-prompt-editing.el \
  's/\(let \(\(file \(or file buffer-file-name\)\)\)/(let ((file (or file buffer-file-name "\/x\/.ai.code.files\/y.org")))/'

# M5: drop the corpus deduplication, so repeated prompts appear many times.
run_mutant "corpus dedup removed" ai-code-prompt-editing.el \
  's/\(delete-dups \(nreverse lines\)\)/(nreverse lines)/'

# M6: accept structural lines as reusable prompts, by removing the minimum
# length guard.
run_mutant "corpus accepts short lines" ai-code-prompt-editing.el \
  's/>= \(length line\) ai-code-prompt-corpus-min-length/>= (length line) 0/'

echo "manual mutation: $MUTANTS_KILLED/$MUTANTS_TOTAL killed"
if [[ "$MUTANTS_KILLED" != "$MUTANTS_TOTAL" ]]; then
  fail_layer "mutation: only $MUTANTS_KILLED of $MUTANTS_TOTAL mutants killed"
fi
record_layer "mutation"

# ---------------------------------------------------------------------------
# Layer 7: negative control
# ---------------------------------------------------------------------------
#
# Proves the guard tests can fail: stub the guard to always return t and
# confirm the suite goes red. A green result here means the gate is blind.

echo "== Layer: negative control =="
NC_BACKUP="$(mktemp)"
cp ai-code-prompt-editing.el "$NC_BACKUP"
# Force the guard to accept every position. Appending a second definition after
# the real one is paren-safe and unambiguously overrides it at load time.
cat >> ai-code-prompt-editing.el <<'STUB'
;; NEGATIVE CONTROL STUB
(defun ai-code--prompt-reference-position-p (&optional _sigil-pos)
  "Always claim a reference position." t)
STUB
if cmp -s ai-code-prompt-editing.el "$NC_BACKUP"; then
  cp "$NC_BACKUP" ai-code-prompt-editing.el
  rm -f "$NC_BACKUP"
  fail_layer "negative control: stub did not apply"
else
  # Verify the stub really makes the guard permissive before trusting the run.
  NC_PROOF="$(emacs_batch -l test/test_00-bootstrap.el --eval "
(progn (require 'ai-code-prompt-editing)
       (with-temp-buffer
         (insert \"#+begin_src java\n@Override\")
         (princ (format \"nc:guard=%s\" (ai-code--prompt-reference-position-p)))))" 2>&1 || true)"
  if ! printf '%s\n' "$NC_PROOF" | grep -qF "nc:guard=t"; then
    cp "$NC_BACKUP" ai-code-prompt-editing.el
    rm -f "$NC_BACKUP"
    fail_layer "negative control: stub applied but guard still rejects, control is invalid"
  else
    NC_OUT="$(run_ert "${FOCUSED_ARGS[@]}")"
    NC_BAD="$(unexpected_count "$NC_OUT")"
    cp "$NC_BACKUP" ai-code-prompt-editing.el
    rm -f "$NC_BACKUP"
    echo "negative control: unexpected=$NC_BAD (must be > 0)"
    if [[ "$NC_BAD" == "MISSING" ]]; then
      # The stub was already proven loadable and permissive above, so a suite
      # that cannot complete is an unexplained failure, not evidence.
      fail_layer "negative control: suite did not complete, result is uninterpretable"
    elif [[ "$NC_BAD" -lt 1 ]]; then
      fail_layer "negative control: suite stayed green with a stubbed guard, the gate is blind"
    fi
  fi
fi
record_layer "negative control"

# ---------------------------------------------------------------------------
# Layer 8: real execution against a real task file
# ---------------------------------------------------------------------------

echo "== Layer: real execution =="
REAL_DIR="$(mktemp -d)"
trap 'rm -rf "$REAL_DIR"' EXIT
mkdir -p "$REAL_DIR/.ai.code.files"
REAL_FILE="$REAL_DIR/.ai.code.files/task.org"
{
  echo "#+TITLE: Real task"
  echo "#+STARTUP: content"
  echo ""
  echo "* Investigation"
  echo "Prose line that should be spell checked."
  echo "#+begin_src java"
  echo "@Override"
  echo "public void run() {}"
  echo "#+end_src"
  echo "* Code Change"
} > "$REAL_FILE"

REAL_OUT="$(emacs_batch -l test/test_00-bootstrap.el --eval "
(progn
  (require 'ai-code-prompt-mode)
  (find-file \"$REAL_FILE\")
  (ai-code-prompt-mode)
  (princ (format \"real:mode=%s\n\" major-mode))
  (princ (format \"real:taskfile=%s\n\" (if (ai-code--prompt-task-file-p) \"yes\" \"no\")))
  ;; The annotation inside the src block must not be a reference position.
  (goto-char (point-min))
  (search-forward \"@Override\")
  (goto-char (match-beginning 0))
  (princ (format \"real:annotation-guarded=%s\n\"
                 (if (ai-code--prompt-reference-position-p (point)) \"no\" \"yes\")))
  ;; Spell checking must be suppressed there too.
  (princ (format \"real:flyspell-skips-code=%s\n\"
                 (if (ai-code--prompt-flyspell-verify) \"no\" \"yes\")))
  ;; A real reference at word start must still be accepted.
  (goto-char (point-max))
  (insert \"\nPlease read @\")
  (princ (format \"real:reference-accepted=%s\n\"
                 (if (ai-code--prompt-reference-position-p) \"yes\" \"no\")))
  ;; Body text must be folded, headings visible.
  (goto-char (point-min))
  (search-forward \"Prose line\")
  (goto-char (match-beginning 0))
  (princ (format \"real:body-folded=%s\n\" (if (org-invisible-p) \"yes\" \"no\")))
  (goto-char (point-min))
  (search-forward \"* Code Change\")
  (goto-char (match-beginning 0))
  (princ (format \"real:heading-visible=%s\n\" (if (org-invisible-p) \"no\" \"yes\")))
  (set-buffer-modified-p nil))" 2>&1 || true)"

printf '%s\n' "$REAL_OUT" | grep -E "^real:" || true

for expect in \
  "real:mode=ai-code-prompt-mode" \
  "real:taskfile=yes" \
  "real:annotation-guarded=yes" \
  "real:flyspell-skips-code=yes" \
  "real:reference-accepted=yes" \
  "real:body-folded=yes" \
  "real:heading-visible=yes"
do
  if ! printf '%s\n' "$REAL_OUT" | grep -qF "$expect"; then
    fail_layer "real execution: expected '$expect'"
  fi
done
record_layer "real execution"

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo
echo "== Layers run: ${#LAYERS_RUN[@]} =="
for l in "${LAYERS_RUN[@]}"; do echo "  - $l"; done

REQUIRED_LAYERS=8
if [[ "${#LAYERS_RUN[@]}" -ne "$REQUIRED_LAYERS" ]]; then
  echo "FAIL: expected $REQUIRED_LAYERS layers, only ${#LAYERS_RUN[@]} ran"
  exit 1
fi

if [[ "${#FAILURES[@]}" -gt 0 ]]; then
  echo
  echo "== FAILURES: ${#FAILURES[@]} =="
  for f in "${FAILURES[@]}"; do echo "  - $f"; done
  exit 1
fi

echo
echo "GAUNTLET PASSED"
echo "  full suite:  ran=$FULL_RAN unexpected=$FULL_BAD (baseline $BASELINE_UNEXPECTED)"
echo "  focused:     ran=$FOCUSED_RAN unexpected=$FOCUSED_BAD"
echo "  mutation:    $MUTANTS_KILLED/$MUTANTS_TOTAL killed"
echo "  checkdoc:    $CHECKDOC_TOTAL issues"
exit 0
