(defpackage #:if-letstar
  (:nicknames #:if-let*)
  (:use #:cl)
  (:export #:if-let*))

(in-package :if-letstar)

(defmacro if-let* (bindings &body bodies)
  "Creates new variable bindings, and conditionally executes either BODY-1 or BODY-2.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each INITIAL-FORM is executed in turn, and the variable bound to the
corresponding value. INITIAL-FORM expressions can refer to variables
previously bound by the IF-LET*.

Execution of IF-LET* causes the form BODY-2 to evaluate if any INITIAL-FORM evaluates to NIL.
If all INITIAL-FORMs evaluate to true, then the form BODY-1 is executed."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,variables
       ;; the butlast and last calls allows this to insert declarations.
       ,@(append (butlast bodies 2)
                 `((if (and
                        ,@(loop for b in binding-list
                                for v in variables
                                ;; the let is here simply to use the malformed let binding error
                                ;; of the host implementation
                                collect `(setq ,v (let (,b) ,v))))
                       ,@(last bodies 2)))))))
