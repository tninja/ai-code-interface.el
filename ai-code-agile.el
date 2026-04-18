;;; ai-code-agile.el --- Agile practices operations for AI Code Interface -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides agile practice operations such as refactoring and TDD cycle
;; for the AI Code Interface package.  Migrated from aider-agile.el.

;;; Code:

(require 'ai-code-input)
(require 'ai-code-prompt-mode)
(require 'thingatpt)
(require 'which-func)

(declare-function ai-code--insert-prompt "ai-code-prompt-mode" (prompt-text))
(declare-function ai-code--get-context-files-string "ai-code-input")
(declare-function ai-code--git-root "ai-code-file" (&optional dir))

(defconst ai-code--refactoring-techniques-catalog
  '((:name "Suggest Refactoring Strategy"
           :scopes (region global)
           :description "Let the LLM analyze the context and suggest the best refactoring technique.")
    (:name "Extract Method"
           :scopes (region global)
           :description "Extract the selected code into a new method named [METHOD_NAME]. Identify parameters and return values needed, and place the new method in an appropriate location."
           :parameters ((:placeholder "[METHOD_NAME]"
                         :prompt "New method name: "
                         :default-fn ai-code--refactoring--method-candidate)))
    (:name "Extract Variable"
           :scopes (region global)
           :description "Replace this complex expression with a well-named variable [VARIABLE_NAME]. Choose a name that clearly explains the expression's purpose."
           :parameters ((:placeholder "[VARIABLE_NAME]"
                         :prompt "New variable name: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Extract Parameter"
           :scopes (region global)
           :description "Extract this expression into a new parameter named [PARAMETER_NAME] for the containing function. Update all call sites to pass this value as an argument."
           :parameters ((:placeholder "[PARAMETER_NAME]"
                         :prompt "New parameter name: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Extract Field"
           :scopes (region global)
           :description "Extract this expression into a class field named [FIELD_NAME]. Initialize the field appropriately and replace the expression with a reference to the field."
           :parameters ((:placeholder "[FIELD_NAME]"
                         :prompt "New field name: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Decompose Conditional"
           :scopes (region global)
           :description "Break down this complex conditional into smaller, more readable pieces. Extract conditions and branches into well-named methods that express the high-level logic.")
    (:name "Replace Nested Conditional with Guard Clauses"
           :scopes (region global)
           :description "Simplify nested conditional logic by using guard clauses. Check for edge cases or simple conditions first and return early.")
    (:name "Replace Magic Number with Symbolic Constant"
           :scopes (region global)
           :description "Replace the selected magic number or string literal with a well-named constant [CONSTANT_NAME]. Define the constant appropriately."
           :parameters ((:placeholder "[CONSTANT_NAME]"
                         :prompt "Constant name: ")))
    (:name "Introduce Assertion"
           :scopes (region global)
           :description "Add an assertion to document an assumption about the program state. Specify the condition to assert [ASSERTION_CONDITION]."
           :parameters ((:placeholder "[ASSERTION_CONDITION]"
                         :prompt "Assertion condition: ")))
    (:name "Consolidate Conditional Expression"
           :scopes (region global)
           :description "Combine multiple conditional checks that lead to the same result into a single, clearer conditional expression.")
    (:name "Inline Method"
           :scopes (region global)
           :description "Replace calls to method [METHOD_NAME] with its body. Ensure the inlining doesn't change behavior or introduce bugs, and remove the original method if it's no longer needed."
           :parameters ((:placeholder "[METHOD_NAME]"
                         :prompt "Method to inline: "
                         :default-fn ai-code--refactoring--method-candidate)))
    (:name "Inline Variable"
           :scopes (region global)
           :description "Replace all references to variable [VARIABLE_NAME] with its value. Ensure the inlining doesn't change behavior or introduce bugs."
           :parameters ((:placeholder "[VARIABLE_NAME]"
                         :prompt "Variable to inline: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Inline Temp"
           :scopes (region global)
           :description "Replace a temporary variable that is assigned once with its expression to simplify the code.")
    (:name "Replace Temp with Query"
           :scopes (region global)
           :description "Replace temporary variables that simply hold calculation results with self-explanatory query methods.")
    (:name "Introduce Explaining Variable"
           :scopes (region global)
           :description "Introduce a well-named variable to clarify a complex expression and improve readability.")
    (:name "Split Temporary Variable"
           :scopes (region global)
           :description "Split a temporary variable that is assigned multiple times into separate variables for each responsibility.")
    (:name "Remove Assignments to Parameters"
           :scopes (region global)
           :description "Avoid reassigning parameters inside a method; instead, use local variables to preserve intent.")
    (:name "Replace Method with Method Object"
           :scopes (region global)
           :description "Turn a complex method into its own object to break down temporary variables and simplify logic.")
    (:name "Substitute Algorithm"
           :scopes (region global)
           :description "Replace an existing algorithm with a clearer or more efficient one while keeping behavior intact.")
    (:name "Move Method"
           :scopes (region global)
           :description "Move method [METHOD_NAME] to class [TARGET_CLASS]. Update all references to use the new location and consider creating a delegation if needed."
           :parameters ((:placeholder "[METHOD_NAME]"
                         :prompt "Method to move: "
                         :default-fn ai-code--refactoring--method-candidate)
                        (:placeholder "[TARGET_CLASS]"
                         :prompt "Target class: ")))
    (:name "Move Field"
           :scopes (region global)
           :description "Relocate a field to the class that makes the most sense for ownership. Update access points and encapsulate if needed.")
    (:name "Extract Class"
           :scopes (region global)
           :description "Extract related fields and methods into a new class named [NEW_CLASS_NAME]. Update the original class to use the new class."
           :parameters ((:placeholder "[NEW_CLASS_NAME]"
                         :prompt "New class name: ")))
    (:name "Inline Class"
           :scopes (region global)
           :description "Merge a class whose responsibilities are too small back into its parent, updating all references.")
    (:name "Hide Delegate"
           :scopes (region global)
           :description "Remove delegation leakage by adding wrapper methods so clients only talk to the main object.")
    (:name "Remove Middle Man"
           :scopes (region global)
           :description "Eliminate unnecessary delegation wrappers and let clients access the related object directly.")
    (:name "Introduce Foreign Method"
           :scopes (region global)
           :description "Add helper methods in client code when you cannot modify the server class to host the logic.")
    (:name "Introduce Local Extension"
           :scopes (region global)
           :description "Extend third-party classes locally via wrappers or subclasses to add behavior safely.")
    (:name "Encapsulate Field"
           :scopes (region global)
           :description "Make the field [FIELD_NAME] private and provide public getter and setter methods for access. Update all direct accesses to use these methods."
           :parameters ((:placeholder "[FIELD_NAME]"
                         :prompt "Field to encapsulate: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Self Encapsulate Field"
           :scopes (region global)
           :description "Access a field through getter and setter methods even inside its own class to simplify future changes.")
    (:name "Replace Data Value with Object"
           :scopes (region global)
           :description "Turn a simple data value into a dedicated object to better express behavior and constraints.")
    (:name "Change Value to Reference"
           :scopes (region global)
           :description "Convert a value object into a shared reference when identity and shared state become important.")
    (:name "Change Reference to Value"
           :scopes (region global)
           :description "Convert a reference object into an immutable value when sharing state is unnecessary.")
    (:name "Replace Array with Object"
           :scopes (region global)
           :description "Replace arrays that mix different kinds of data with well-named objects and fields.")
    (:name "Duplicate Observed Data"
           :scopes (region global)
           :description "Maintain local copies of observed data so domain logic can work without tight coupling to the observer.")
    (:name "Change Unidirectional Association to Bidirectional"
           :scopes (region global)
           :description "Introduce links in both directions when objects need to navigate to each other.")
    (:name "Change Bidirectional Association to Unidirectional"
           :scopes (region global)
           :description "Remove unnecessary reverse links when only one-way navigation is required.")
    (:name "Encapsulate Collection"
           :scopes (region global)
           :description "Ensure collection fields return read-only views and expose modifier methods to protect invariants.")
    (:name "Replace Type Code with Class"
           :scopes (region global)
           :description "Replace primitive type codes with dedicated classes to capture behavior and validation.")
    (:name "Replace Type Code with Subclasses"
           :scopes (region global)
           :description "Substitute type codes with subclasses to leverage polymorphism for specialized behavior.")
    (:name "Replace Type Code with State/Strategy"
           :scopes (region global)
           :description "Transform a type code into State or Strategy objects to vary behavior dynamically.")
    (:name "Replace Subclass with Fields"
           :scopes (region global)
           :description "Flatten simple subclasses by replacing them with fields when inheritance no longer adds value.")
    (:name "Consolidate Duplicate Conditional Fragments"
           :scopes (region global)
           :description "Move repeated code inside conditionals to a single location executed in all paths.")
    (:name "Remove Control Flag"
           :scopes (region global)
           :description "Eliminate control flags by using `return`, `break`, or `continue` to control flow more directly.")
    (:name "Introduce Null Object"
           :scopes (region global)
           :description "Introduce a null object that encapsulates the default do-nothing behavior instead of handling null checks.")
    (:name "Rename Variable/Method"
           :scopes (region global)
           :description "Rename [CURRENT_NAME] to [NEW_NAME]. Ensure all references are updated consistently following naming conventions appropriate for this codebase."
           :parameters ((:placeholder "[CURRENT_NAME]"
                         :prompt "Current name: "
                         :default-fn ai-code--refactoring--symbol-candidate)
                        (:placeholder "[NEW_NAME]"
                         :prompt-fn ai-code--refactoring--rename-new-name-prompt)))
    (:name "Change Method Signature"
           :scopes (region global)
           :description "Update method [METHOD_NAME] signature to [NEW_SIGNATURE]. Adjust all call sites, documentation, and overloads to match the new contract."
           :parameters ((:placeholder "[METHOD_NAME]"
                         :prompt "Method to change signature: "
                         :default-fn ai-code--refactoring--method-candidate)
                        (:placeholder "[NEW_SIGNATURE]"
                         :prompt "Describe the new signature (parameters, return type, etc.): ")))
    (:name "Introduce Constant"
           :scopes (region global)
           :description "Introduce a named constant [CONSTANT_NAME] for the selected literal or expression, choose the proper scope, and replace relevant usages."
           :parameters ((:placeholder "[CONSTANT_NAME]"
                         :prompt "Constant name: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Introduce Field"
           :scopes (region global)
           :description "Promote the selected value to a class field named [FIELD_NAME], decide initialization timing, and replace repeated computations."
           :parameters ((:placeholder "[FIELD_NAME]"
                         :prompt "Introduced field name: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Introduce Property"
           :scopes (region global)
           :description "Wrap field [FIELD_NAME] with a property named [PROPERTY_NAME], generating accessor logic that follows project conventions."
           :parameters ((:placeholder "[FIELD_NAME]"
                         :prompt "Existing field to wrap: "
                         :default-fn ai-code--refactoring--symbol-candidate)
                        (:placeholder "[PROPERTY_NAME]"
                         :prompt "Property name: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Safe Delete"
           :scopes (region global)
           :description "Remove [ELEMENT_NAME] only after verifying there are no remaining usages. Update dependent code or tests to keep the project consistent."
           :parameters ((:placeholder "[ELEMENT_NAME]"
                         :prompt "Element to delete safely: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Convert Anonymous Class to Lambda"
           :scopes (region global)
           :description "Convert the selected anonymous class implementing [INTERFACE_NAME] into an equivalent lambda expression or method reference if the language supports it."
           :parameters ((:placeholder "[INTERFACE_NAME]"
                         :prompt "Functional interface or delegate type: "
                         :default-fn ai-code--refactoring--symbol-candidate)))
    (:name "Add Parameter"
           :scopes (region global)
           :description "Add a new parameter to a method to supply the data it actually needs, updating all callers.")
    (:name "Remove Parameter"
           :scopes (region global)
           :description "Eliminate a parameter that is no longer used and update all callers accordingly.")
    (:name "Separate Query from Modifier"
           :scopes (region global)
           :description "Split a method that both queries and alters state into two methods with single responsibilities.")
    (:name "Parameterize Method"
           :scopes (region global)
           :description "Turn similar methods that differ only in values into a single method that accepts parameters for the variation.")
    (:name "Replace Parameter with Explicit Methods"
           :scopes (region global)
           :description "Replace a parameter that selects different behaviors with explicitly named methods for each case.")
    (:name "Preserve Whole Object"
           :scopes (region global)
           :description "Pass the entire object to a method instead of individual fields so related data travels together.")
    (:name "Replace Parameter with Method"
           :scopes (region global)
           :description "Remove parameters that can be derived inside the method and call helper queries instead.")
    (:name "Introduce Parameter Object"
           :scopes (region global)
           :description "Replace these related parameters with a single parameter object named [OBJECT_NAME]. Create an appropriate class for the parameter object."
           :parameters ((:placeholder "[OBJECT_NAME]"
                         :prompt "Parameter object name: ")))
    (:name "Remove Setting Method"
           :scopes (region global)
           :description "Eliminate setter methods for fields that should be immutable after construction.")
    (:name "Hide Method"
           :scopes (region global)
           :description "Reduce the visibility of methods that are only intended for internal use.")
    (:name "Replace Constructor with Factory Method"
           :scopes (region global)
           :description "Wrap object creation in a factory method when more descriptive names or varied creation logic is required.")
    (:name "Replace Error Code with Exception"
           :scopes (region global)
           :description "Throw an exception instead of returning an error code to signal failure states.")
    (:name "Replace Exception with Test"
           :scopes (region global)
           :description "Perform explicit checks before performing work to avoid using exceptions for normal control flow.")
    (:name "Pull Up Method"
           :scopes (region global)
           :description "Move method [METHOD_NAME] from the current class to its superclass [SUPERCLASS_NAME]. Ensure the method is applicable to the superclass context."
           :parameters ((:placeholder "[METHOD_NAME]"
                         :prompt "Method to pull up: "
                         :default-fn ai-code--refactoring--method-candidate)
                        (:placeholder "[SUPERCLASS_NAME]"
                         :prompt "Superclass name: ")))
    (:name "Pull Up Field"
           :scopes (region global)
           :description "Move common fields from subclasses into the superclass to centralize shared state.")
    (:name "Pull Up Constructor Body"
           :scopes (region global)
           :description "Move duplicated constructor logic from subclasses into the superclass constructor.")
    (:name "Push Down Method"
           :scopes (region global)
           :description "Move method [METHOD_NAME] from the current class to specific subclass(es) [SUBCLASS_NAMES] where it is actually used."
           :parameters ((:placeholder "[METHOD_NAME]"
                         :prompt "Method to push down: "
                         :default-fn ai-code--refactoring--method-candidate)
                        (:placeholder "[SUBCLASS_NAMES]"
                         :prompt "Comma-separated subclass names: ")))
    (:name "Push Down Field"
           :scopes (region global)
           :description "Move fields that are only used in some subclasses down into those specific subclasses.")
    (:name "Extract Subclass"
           :scopes (region global)
           :description "Create a new subclass when a class has behavior or data used only in some instances.")
    (:name "Extract Superclass"
           :scopes (region global)
           :description "Factor out common behavior or data into a new superclass shared by multiple classes.")
    (:name "Extract Interface"
           :scopes (region global)
           :description "Introduce an interface to capture common protocol without sharing implementation.")
    (:name "Collapse Hierarchy"
           :scopes (region global)
           :description "Flatten inheritance when subclass and superclass differ too little to justify separation.")
    (:name "Form Template Method"
           :scopes (region global)
           :description "Create a template method in a superclass to outline an algorithm while subclasses fill in the steps.")
    (:name "Replace Inheritance with Delegation"
           :scopes (region global)
           :description "Swap inheritance for delegation when only part of the behavior should be reused.")
    (:name "Replace Delegation with Inheritance"
           :scopes (region global)
           :description "Simplify a delegation chain by adopting inheritance when the relationship is truly is-a.")
    (:name "Tease Apart Inheritance"
           :scopes (global)
           :description "Separate intertwined inheritance hierarchies so each hierarchy represents a single responsibility.")
    (:name "Convert Procedural Design to Objects"
           :scopes (global)
           :description "Restructure procedural code into cohesive objects that encapsulate data and behavior.")
    (:name "Separate Domain from Presentation"
           :scopes (global)
           :description "Split domain logic from UI or presentation concerns to improve testability and reuse.")
    (:name "Extract Hierarchy"
           :scopes (global)
           :description "Introduce a new hierarchy to clarify different responsibilities and support future extension."))
  "Catalog of refactoring techniques curated from Martin Fowler's \"Refactoring\".")

(defconst ai-code--refactoring-suggestion-default-instruction
  "Analyze the code context below. Identify potential refactoring opportunities (e.g., complexity, duplication, clarity) and make the code easy to understand. Do not change code logic. Suggest the most impactful refactoring technique and explain why."
  "Default instruction for refactoring suggestion prompts.")

(defconst ai-code--refactoring-suggestion-default-label
  "General refactoring analysis"
  "Default short description for refactoring suggestion prompts.")

(defconst ai-code--refactoring-suggestion-presets
  `((,ai-code--refactoring-suggestion-default-label . ,ai-code--refactoring-suggestion-default-instruction)
    ("Improve readability and testability" . "Analyze the code context below. Focus on making the code easier to understand, improving readability, and increasing testability. Do not change code logic. Suggest the most impactful refactoring technique and explain why.")
    ("Reduce complexity" . "Analyze the code context below. Focus on reducing complexity and simplifying control flow. Do not change code logic. Suggest the most impactful refactoring technique and explain why.")
    ("Remove duplication" . "Analyze the code context below. Focus on removing duplication and consolidating repeated logic. Do not change code logic. Suggest the most impactful refactoring technique and explain why.")
    ("Clarify naming and responsibilities" . "Analyze the code context below. Focus on clarifying naming and separating responsibilities more cleanly. Do not change code logic. Suggest the most impactful refactoring technique and explain why."))
  "Preset refactoring suggestion prompts keyed by short description.")

(defun ai-code--read-refactoring-suggestion-instruction ()
  "Read a refactoring suggestion instruction with editable completion."
  (let* ((selected-description
          (completing-read "Select refactoring goal: "
                           (mapcar #'car ai-code--refactoring-suggestion-presets)
                           nil t nil nil ai-code--refactoring-suggestion-default-label))
         (default-instruction
          (or (cdr (assoc selected-description ai-code--refactoring-suggestion-presets))
              ai-code--refactoring-suggestion-default-instruction)))
    (ai-code-read-string "Edit suggestion request: " default-instruction)))

(defun ai-code--refactoring--ensure-string (value)
  "Return VALUE coerced to a string when appropriate."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun ai-code--refactoring--method-candidate (context _values)
  "Suggest a method name based on CONTEXT or point."
  (ai-code--refactoring--ensure-string
   (or (plist-get context :current-function)
       (thing-at-point 'symbol t))))

(defun ai-code--refactoring--symbol-candidate (_context _values)
  "Suggest a symbol name at point."
  (ai-code--refactoring--ensure-string
   (thing-at-point 'symbol t)))

(defun ai-code--refactoring--find-technique (technique-name)
  "Find technique entry in catalog by TECHNIQUE-NAME."
  (catch 'found
    (dolist (entry ai-code--refactoring-techniques-catalog nil)
      (when (string= technique-name (plist-get entry :name))
        (throw 'found entry)))))

(defun ai-code--refactoring--get-placeholder (values placeholder)
  "Retrieve previously captured PLACEHOLDER value from VALUES."
  (cdr (assoc placeholder values)))

(defun ai-code--refactoring--rename-new-name-prompt (_context values _default)
  "Build prompt for rename using VALUES."
  (let ((current (ai-code--refactoring--get-placeholder values "[CURRENT_NAME]")))
    (if current
        (format "Rename '%s' to: " current)
      "New name: ")))

(defun ai-code--refactoring--resolve-parameter (spec context values)
  "Resolve parameter value defined by SPEC using CONTEXT and VALUES."
  (let* ((value-fn (plist-get spec :value-fn))
         (value (if value-fn
                    (funcall value-fn context values)
                  (let* ((default (ai-code--refactoring--ensure-string
                                   (let ((default-fn (plist-get spec :default-fn)))
                                     (if default-fn
                                         (funcall default-fn context values)
                                       (plist-get spec :default)))))
                         (raw-prompt (let ((prompt-fn (plist-get spec :prompt-fn)))
                                       (cond
                                        (prompt-fn (funcall prompt-fn context values default))
                                        ((plist-get spec :prompt) (plist-get spec :prompt))
                                        (t nil))))
                         (prompt (if raw-prompt
                                     (concat raw-prompt "(leave empty or type 'default' for AI to decide) ")
                                   nil))
                         (user-input (if prompt
                                         (ai-code-read-string prompt "default")
                                       "default")))
                    (if (or (null user-input)
                            (string= "" user-input)
                            (string= "default" user-input))
                        ""
                      user-input)))))
    (ai-code--refactoring--ensure-string value)))

(defun ai-code--refactoring-dired-has-explicit-marks-p (all-marked file-at-point)
  "Return non-nil when ALL-MARKED records explicit Dired selections.
A single FILE-AT-POINT entry means Dired is only reporting the current line."
  (and all-marked
       (not (and (= (length all-marked) 1)
                 file-at-point
                 (equal (car all-marked) file-at-point)))))

(defun ai-code--refactoring-dired-targets ()
  "Return selected Dired targets for refactoring."
  (let* ((all-marked (dired-get-marked-files))
         (file-at-point (dired-get-filename nil t))
         (has-marks (ai-code--refactoring-dired-has-explicit-marks-p
                     all-marked file-at-point)))
    (cond
     (has-marks all-marked)
     (file-at-point (list file-at-point))
     (t (user-error "No file or directory selected in Dired")))))

(defun ai-code--refactoring-targets-git-root (targets)
  "Return Git root for refactoring TARGETS or current directory."
  (or (ai-code--git-root (car targets))
      (ai-code--git-root default-directory)))

(defun ai-code--get-refactoring-context ()
  "Get the current context for refactoring."
  (let* ((dired-targets (when (derived-mode-p 'dired-mode)
                          (ai-code--refactoring-dired-targets)))
         (region-active (and (not dired-targets) (region-active-p)))
         (current-function (unless dired-targets (which-function)))
         (file-name (unless dired-targets
                      (when buffer-file-name
                        (file-name-nondirectory buffer-file-name)))))
    (list :region-active region-active
          :region-text (when region-active
                         (buffer-substring-no-properties (region-beginning) (region-end)))
          :current-function current-function
          :file-name file-name
          :dired-targets dired-targets
          :context-description (cond
                                (dired-targets
                                 (if (= (length dired-targets) 1)
                                     (format "for '%s'"
                                             (file-name-nondirectory
                                              (directory-file-name (car dired-targets))))
                                   "for selected files/directories"))
                                (current-function (format "in function '%s'" current-function))
                                (file-name (format "in file '%s'" file-name))
                                (t "in current context")))))

(defun ai-code--refactoring-context-files-string (context)
  "Return a refactoring file context string for CONTEXT.
Uses @-prefixed Git-relative paths when a Git root is available,
otherwise falls back to absolute paths."
  (let ((dired-targets (plist-get context :dired-targets)))
    (if dired-targets
        (let* ((git-root (ai-code--refactoring-targets-git-root dired-targets))
               (paths (mapcar (lambda (path)
                                (if git-root
                                    (concat "@" (file-relative-name path git-root))
                                  path))
                              dired-targets)))
          (concat "\nFiles:\n" (mapconcat #'identity paths "\n")))
      (ai-code--get-context-files-string))))

(defun ai-code--get-refactoring-techniques (region-active)
  "Return appropriate refactoring techniques based on REGION-ACTIVE."
  (let ((scope (if region-active 'region 'global))
        (result nil))
    (dolist (entry ai-code--refactoring-techniques-catalog (nreverse result))
      (when (memq scope (plist-get entry :scopes))
        (push (cons (plist-get entry :name)
                    (plist-get entry :description))
              result)))))

(defun ai-code--process-refactoring-parameters (selected-technique technique-description context)
  "Process parameters for SELECTED-TECHNIQUE using CONTEXT.
TECHNIQUE-DESCRIPTION is the base prompt text."
  (let* ((entry (ai-code--refactoring--find-technique selected-technique))
         (parameters (plist-get entry :parameters))
         (final-description technique-description)
         (values nil))
    ;; First, resolve all parameter values and store them.
    (dolist (spec parameters)
      (let* ((placeholder (plist-get spec :placeholder))
             (value (ai-code--refactoring--resolve-parameter spec context values)))
        (when placeholder
          (push (cons placeholder value) values))))

    ;; Second, iterate through the resolved values and perform substitutions.
    ;; Process in reverse order of parameter definition.
    ;; Rationale: If a placeholder is a substring of another, or if the prompt contains
    ;; optional clauses (e.g., "to <target> with <modifier>"), substituting in forward order
    ;; can cause incorrect replacements or leave behind awkward text when a placeholder is empty.
    ;; For example, given the prompt "Rename <entity> to <target> with <modifier>", if <target>
    ;; is empty and we process <entity> first, we might replace <entity> and then be left with
    ;; "Rename foo to  with <modifier>", making it harder to cleanly remove "to ".
    ;; By processing in reverse order, we remove or replace the later placeholders first,
    ;; ensuring that any optional text (like "to", "with") associated with an empty placeholder
    ;; is also removed, resulting in a more natural prompt.
    ;; Example:
    ;;   Prompt: "Rename <entity> to <target> with <modifier>"
    ;;   Parameters: <entity>="foo", <target>="", <modifier>="logging"
    ;;   Result: "Rename foo with logging"
    (dolist (param-pair (nreverse values) final-description)
      (let* ((placeholder (car param-pair))
             (resolved-value (cdr param-pair)))
        (if (string= "" resolved-value)
            ;; If resolved value is empty, remove the placeholder and potentially
            ;; any preceding descriptive text like " to ", " named ", " with ".
            ;; The regex accounts for optional whitespace and common prepositions/conjunctions.
            (setq final-description (replace-regexp-in-string
                                     (concat "\\s-*\\(?:to\\|named\\|with\\|for\\|in\\)?\\s-*\\(" (regexp-quote placeholder) "\\)")
                                     ""
                                     final-description
                                     t t))
          ;; If resolved value is not empty, replace the placeholder with its value.
          (setq final-description (replace-regexp-in-string
                                   (regexp-quote placeholder)
                                   resolved-value
                                   final-description
                                   t t)))))
    ;; Finally, clean up any multiple spaces that might have been introduced
    ;; and trim leading/trailing spaces.
    (setq final-description (replace-regexp-in-string "  +" " " final-description t t))
    (string-trim final-description)))

(defun ai-code--handle-specific-refactoring (selected-technique all-techniques context tdd-mode)
  "Handle the case where a specific refactoring technique is chosen.
Uses SELECTED-TECHNIQUE, ALL-TECHNIQUES, CONTEXT, and TDD-MODE.
If TDD-MODE is non-nil, adds TDD constraints to the instruction."
  (let* ((region-active (plist-get context :region-active))
         (region-text (plist-get context :region-text))
         (context-description (plist-get context :context-description))
         (technique-description (cdr (assoc selected-technique all-techniques)))
         (prompt-with-params (ai-code--process-refactoring-parameters
                              selected-technique technique-description context))
         (base-instruction (format "%s %s. %s"
                                   selected-technique
                                   context-description
                                   prompt-with-params))
         ;; Add TDD constraint if in TDD mode
         (tdd-constraint (if tdd-mode " Ensure all tests still pass after refactoring." ""))
         (initial-instruction (concat base-instruction tdd-constraint ". Go ahead and make the code change."))
         (final-instruction (ai-code-read-string "Edit refactoring instruction: " initial-instruction))
         ;; Add file information to context
         (file-info (ai-code--get-context-files-string))
         (command (if region-active
                      (format "%s%s\n\nSelected code:\n%s"
                              final-instruction
                              file-info
                              region-text)
                    (format "%s%s" final-instruction file-info)))
         (message-suffix (if tdd-mode " during TDD refactor stage" "")))
    (when (ai-code--insert-prompt command)
      (message "%s refactoring request sent to AI Code Interface%s. After code refactored, better to re-run unit-tests."
               selected-technique message-suffix))))

(defun ai-code--handle-ask-llm-suggestion (context tdd-mode)
  "Handle the case where the user asks the LLM for a refactoring suggestion.
Uses CONTEXT and TDD-MODE.
If TDD-MODE is non-nil, adds TDD constraints to the prompt."
  (let* ((region-active (plist-get context :region-active))
         (region-text (plist-get context :region-text))
         (current-function (plist-get context :current-function))
         (dired-targets (plist-get context :dired-targets))
         (context-info (cond
                        (region-active "Selected code region")
                        (dired-targets "Selected files/directories")
                        (current-function (format "Function '%s'" current-function))
                        (t ;; use current buffer file name when no region/function
                         (let ((fname (plist-get context :file-name)))
                           (format "File '%s'" (or fname "All added files"))))))
         (code-snippet (if region-active
                           (format "\n```\n%s\n```" region-text)
                         ""))
                ;; Get the main instruction from the user
                 (user-instruction (ai-code--read-refactoring-suggestion-instruction))
                 ;; Add TDD constraint if in TDD mode
                 (tdd-constraint (if tdd-mode " Ensure all tests still pass after refactoring." ""))
                 ;; Add file information to context
                 (file-info (ai-code--refactoring-context-files-string context))
                 ;; Construct the prompt using user input and context
         (base-prompt (format "%s Context: %s%s%s"
                              user-instruction
                              context-info
                              file-info
                             code-snippet))
        (prompt (concat base-prompt tdd-constraint))
        (message-suffix (if tdd-mode " during TDD refactor stage" "")))
    ;; Send the prompt using the ai-code--insert-prompt function
    (when (ai-code--insert-prompt prompt)
      ;; Inform the user
      (message "Requesting refactoring suggestion from AI Code Interface%s. If you are happy with the suggestion, use 'go ahead' to accept the change"
               message-suffix))))

;;;###autoload
(defun ai-code-refactor-book-method (&optional tdd-mode)
  "Apply refactoring techniques or request suggestions.
Uses current context (function, class, selected region).
If TDD-MODE is non-nil, adjusts prompts and instructions for the
TDD refactor stage."
  ;; The `interactive` spec needs to handle the optional argument if called directly,
  ;; but here it's primarily called programmatically from ai-code-tdd-cycle or interactively without args.
  ;; For interactive calls, tdd-mode will be nil.
  (interactive)
  (let* ((context (ai-code--get-refactoring-context))
         (dired-targets (plist-get context :dired-targets))
         (region-active (plist-get context :region-active)))
    (if dired-targets
        (ai-code--handle-ask-llm-suggestion context tdd-mode)
      (let* ((all-techniques (ai-code--get-refactoring-techniques region-active))
             (technique-names (mapcar #'car all-techniques))
             (prompt-prefix (if tdd-mode "Select TDD refactoring technique" "Select refactoring technique"))
             (prompt-suffix (if region-active " for selected region: " ": "))
             (prompt (concat prompt-prefix prompt-suffix))
             (selected-technique (completing-read prompt technique-names nil t)))
        ;; Dispatch to appropriate handler based on user selection
        (if (string= selected-technique "Suggest Refactoring Strategy")
            (ai-code--handle-ask-llm-suggestion context tdd-mode)
          (ai-code--handle-specific-refactoring selected-technique all-techniques context tdd-mode))))))

(defun ai-code--ensure-test-buffer-visible ()
  "Ensure that at least one buffer in the current windows is a test file.
A test file is identified by having \\='test\\=' in its name (case insensitive).
If no such buffer is found, report a user-error."
  (let ((has-test-buffer nil)
        (case-fold-search t))
    (dolist (win (window-list))
      (when (string-match-p "test" (buffer-name (window-buffer win)))
        (setq has-test-buffer t)))
    ;; test buffer is must need. non-test buffer is not a must need.
    ;; since test buffer is sufficient for bootstrapping
    (unless has-test-buffer
      (user-error "No test file found in current windows.  Please open a test file first"))))

(defconst ai-code--tdd-test-pattern-instruction
  "\nFollow the test-code pattern in the current project. Write the test-code in the test-file. If the test-file does not exist, create it using the same test-filename pattern used in this repository."
  "Instruction appended to TDD prompts to enforce the project's test pattern.")

(defconst ai-code--tdd-run-test-after-this-stage-instruction
  " Run test after this stage and output the summary of test result. State whether the result matches the goal of this stage. List the files changed and exact test command / result. List the public API / log key / config key change if there is."
  "Instruction appended to single-stage TDD prompts.")

(defconst ai-code--tdd-run-test-after-each-stage-instruction
  " Run test after each stage and output the summary of test result. For each stage, list the stage name, files changed, exact test command / result, and whether the result matches the goal of that stage. List the public API / log key / config key change if there is."
  "Instruction appended to multi-stage TDD prompts.")

(defconst ai-code--tdd-red-green-base-instruction
  " Follow strict TDD stages. Do not skip stages. Stage 1 - Red: update only test code and write the smallest failing test that captures the requested behavior. Do not modify source code during Red. Stage 2 - Green: after confirming the new test fails for the expected reason, update the minimum source code needed to make it pass. Do not refactor during Green."
  "Base instruction shared by Red+Green style TDD prompts.")

(defconst ai-code--tdd-red-green-tail-instruction
  " Keep the changes narrowly scoped to the requested behavior. Only update the relevant test and source code. Do not add extra features or unrelated cleanup."
  "Trailing instruction shared by Red+Green style TDD prompts.")

(defconst ai-code--tdd-with-refactoring-extension-instruction
  " Stage 3 - Blue: after Green is passing, refactor only the files changed in Red/Green. Preserve behavior and do not add features. First review the code diff (including tests) and identify the highest-impact cleanup. Then apply focused refactoring that improves readability, keeps classes/functions small and cohesive / easy to test, reduces duplication, and simplifies naming and control flow."
  "Refactoring extension shared by Red+Green+Blue style TDD prompts.")

(defun ai-code--tdd-red-stage (function-name)
  "Handle the Red stage of TDD for FUNCTION-NAME: Write a failing test."
  (let ((test-pattern-instruction ai-code--tdd-test-pattern-instruction))
    (if (and (region-active-p) (not (derived-mode-p 'prog-mode)))
        ;; If there is a selected region, and it is not a prog-mode derived buffer,
        ;; assume this is the information / exception of failed test.
        ;; Use it as initial prompt to fix the source code.
        (let* ((failure-info (buffer-substring-no-properties (region-beginning) (region-end)))
               (prompt (ai-code-read-string "Confirm test failure to fix: " failure-info))
               (file-info (ai-code--get-context-files-string))
               (tdd-instructions (format "Fix the code to resolve the following error:\n%s%s%s"
                                         prompt
                                         file-info
                                         test-pattern-instruction)))
          (ai-code--insert-prompt tdd-instructions))
      ;; Original path: write a failing test
      (ai-code--ensure-test-buffer-visible)
      (let* ((feature-desc (ai-code-read-string
                            (if function-name
                                (format "Describe the feature to test for '%s': " function-name)
                              "Describe the feature to test: ") "Implement test functions using test cases described in the comments."))
             (file-info (ai-code--get-context-files-string))
             (tdd-instructions
              (format "%s%s\nFollow TDD principles - write only the test now, not the implementation. The test should fail when run because the functionality doesn't exist yet. Only update test file code.%s"
                      feature-desc
                      file-info
                      (concat ai-code--tdd-run-test-after-this-stage-instruction
                              test-pattern-instruction))))
        (ai-code--insert-prompt tdd-instructions)))))

(defun ai-code--tdd-source-function-context-p (function-name)
  "Return non-nil when FUNCTION-NAME is in a non-test source buffer."
  (and function-name
       buffer-file-name
       (derived-mode-p 'prog-mode)
       (let ((case-fold-search t))
         (not (string-match-p "test" (file-name-nondirectory buffer-file-name))))))

(defun ai-code--write-test (function-name)
  "Write a test for FUNCTION-NAME in the corresponding test file."
  (let* ((source-file (and buffer-file-name (file-name-nondirectory buffer-file-name)))
         (test-file-hint (if source-file
                             (format "the corresponding test file for %s, following test pattern of this repo"
                                     source-file)
                           "the corresponding test file"))
         (write-test-desc (ai-code-read-string
                           "Write test instruction: "
                           (format "Write test for '%s' in %s."
                                   function-name
                                   test-file-hint)))
         (file-info (ai-code--get-context-files-string))
         (tdd-instructions
          (format "%s%s\nFollow TDD principles - write only the test now, not the implementation. Only update test file code.%s"
                  write-test-desc
                  file-info
                  ai-code--tdd-test-pattern-instruction)))
    (ai-code--insert-prompt tdd-instructions)))

(defun ai-code--tdd-red-green-stage (function-name)
  "Handle the Red + Green stage for FUNCTION-NAME in one prompt."
  ;; (ai-code--ensure-test-buffer-visible)
  (let* ((feature-desc (ai-code-read-string
                        (if function-name
                            (format "Describe the feature to test for '%s': " function-name)
                          "Describe the feature to test: ")
                        "Implement test functions using test cases described in the comments."))
         (file-info (ai-code--get-context-files-string))
         (tdd-instructions
          (format "%s%s\n%s%s%s%s"
                  feature-desc
                  file-info
                  ai-code--tdd-red-green-base-instruction
                  ai-code--tdd-red-green-tail-instruction
                  ai-code--tdd-run-test-after-each-stage-instruction
                  ai-code--tdd-test-pattern-instruction)))
    (ai-code--insert-prompt tdd-instructions)))

(defun ai-code--tdd-red-green-blue-stage (function-name)
  "Handle the Red + Green + Blue stage for FUNCTION-NAME in one prompt."
  ;; (ai-code--ensure-test-buffer-visible)
  (let* ((feature-desc (ai-code-read-string
                        (if function-name
                            (format "Describe the feature to test for '%s': " function-name)
                          "Describe the feature to test: ")
                        "Implement test functions using test cases described in the comments."))
         (file-info (ai-code--get-context-files-string))
         (tdd-instructions
          (format "%s%s\n%s%s%s%s%s"
                  feature-desc
                  file-info
                  ai-code--tdd-red-green-base-instruction
                  ai-code--tdd-with-refactoring-extension-instruction
                  ai-code--tdd-red-green-tail-instruction
                  ai-code--tdd-run-test-after-each-stage-instruction
                  ai-code--tdd-test-pattern-instruction)))
    (ai-code--insert-prompt tdd-instructions)))

(defun ai-code--tdd-green-stage (function-name)
  "Handle the Green stage of TDD for FUNCTION-NAME: Make the test pass.
If current file is a test file (contains \\='test\\=' in name), provide prompt
to fix code."
  (ai-code--ensure-test-buffer-visible)
  (let* ((is-test-buffer (and (buffer-file-name) (string-match-p "test" (buffer-file-name))))
         (initial-input
          (if is-test-buffer
              (format "Current test file: %s\ntest function: %s\n is failing. Please fix the code to make the test pass.\nTest failure details: "
                      (file-name-nondirectory (buffer-file-name))
                      (or function-name "some test functions"))
            (if function-name
                (format "Implement function '%s' to make tests pass: " function-name)
              "Implement the minimal code needed to make the failing test pass: ")))
         (implementation-desc (ai-code-read-string "Implementation instruction: " initial-input))
         (file-info (ai-code--get-context-files-string))
         (tdd-instructions
          (format "%s%s\nFollow TDD principles - implement the code needed to make the test pass.%s"
                  implementation-desc
                  file-info
                  ai-code--tdd-run-test-after-this-stage-instruction)))
    (ai-code--insert-prompt tdd-instructions)))

(defun ai-code--run-test-ai-assisted ()
  "Send a prompt to AI to run a test command with current context."
  (let* ((is-dired (derived-mode-p 'dired-mode))
         (function-name (unless is-dired (which-function)))
         (file-info (unless is-dired (ai-code--get-context-files-string)))
         (error-handling-instruction
          (concat "\n\nIf any test fails:"
                  "\n1. Analyze the test failure output carefully to identify which test(s) failed"
                  "\n2. Investigate the root cause by examining the test code and related source files"
                  "\n3. Provide a clear explanation of what went wrong"
                  "\n4. Suggest specific code fixes for user approval before making any changes"))
         (initial-input
          (cond
           (is-dired
            (format "Run the tests for source code in directory '%s' using appropriate test runner.%s"
                    (dired-current-directory)
                    error-handling-instruction))
           (function-name
            (format "Run the tests for the current function '%s' using appropriate test runner.%s%s"
                    function-name
                    file-info
                    error-handling-instruction))
           (t
            (format "Run the tests for the current file using appropriate test runner.%s%s"
                    file-info
                    error-handling-instruction))))
         (prompt (ai-code-read-string "Send to AI: " initial-input)))
    (ai-code--insert-prompt prompt)))

;;;###autoload
(defun ai-code-run-test ()
  "Ask AI to run the relevant tests for the current context."
  ;; DONE: remove pytest / jest / ert usage here, simply call ai-code--run-test-ai-assisted
  ;; DONE: Remove related information from README / autoloads / @test as well, since we want to keep the @test running experience consistent across languages and rely on AI to figure out the test pattern and command.
  (interactive)
  (ai-code--run-test-ai-assisted))

;;;###autoload
(defun ai-code-tdd-cycle ()
  "Guide through Test Driven Development cycle (Red-Green-Refactor).
Helps users follow Kent Beck's TDD methodology with AI assistance.
Works with both source code and test files that have been added to ai-code."
  (interactive)
  (let* ((function-name (which-function))
         (use-write-test-stage (ai-code--tdd-source-function-context-p function-name))
         (red-stage-label (if use-write-test-stage
                              (format "1. Red (Write test for %s)" function-name)
                            "1. Red (Write failing test)"))
         (cycle-stage (completing-read
                       "Select TDD stage: "
                       (list "0. Run unit-tests"
                             red-stage-label
                             "2. Green (Make test pass)"
                             "3. Blue (Refactor, improve code quality)"
                             "4. Red + Green (One prompt)"
                             "5. Red + Green + Blue (One prompt)")
                       nil t))
         (stage-num (string-to-number (substring cycle-stage 0 1))))
    (cond
     ;; Run tests
     ((= stage-num 0) (ai-code-run-test))
     ;; Red stage - write failing test
     ((= stage-num 1)
      (if use-write-test-stage
          (ai-code--write-test function-name)
        (ai-code--tdd-red-stage function-name)))
     ;; Green stage - make test pass
     ((= stage-num 2) (ai-code--tdd-green-stage function-name))
     ;; Refactor stage - call the main refactoring function in TDD mode
     ((= stage-num 3) (ai-code-refactor-book-method t))
     ;; Red + Green combined in one prompt
     ((= stage-num 4) (ai-code--tdd-red-green-stage function-name))
     ;; Red + Green + Blue combined in one prompt
     ((= stage-num 5) (ai-code--tdd-red-green-blue-stage function-name)))))

(provide 'ai-code-agile)
;;; ai-code-agile.el ends here
