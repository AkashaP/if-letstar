(defpackage #:if-letstar
  (:nicknames #:if-let*)
  (:use #:cl)
  (:export #:if-let*
           #:if-let 
           #:when-let
           #:when-let*))

(in-package :if-letstar)

(defmacro if-let* (bindings &body bodies)
  "Creates new symbol bindings, and conditionally executes either the second-last or last form of BODIES.

BINDINGS must be either single binding of the form:

 (symbol initial-form)

or a list of bindings of the form:

 ((symbol-1 initial-form-1)
  (symbol-2 initial-form-2)
  ...
  (symbol-n initial-form-n))

Each INITIAL-FORM is executed in turn, and the variable bound to the
corresponding value. INITIAL-FORM expressions can refer to symbols
previously bound by the IF-LET*.

Any declarations can come below the bindings form, before the start of any significant code in BODIES.

Execution of IF-LET* causes the last form of BODIES to evaluate if any INITIAL-FORM evaluates to NIL.
If all INITIAL-FORMs evaluate to true, then the second-last form of BODIES is executed."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,variables
       ,@(remove nil ; don't leave random floating nils around that nil don't do nil nil anything
                 `(,@(butlast bodies 2) ;insert declarations
                   (if (and
                        ,@(loop for b in binding-list
                                for v in variables
                                ;; the let is here simply to use the malformed let binding error
                                ;; of the host implementation
                                collect `(setq ,v (let (,b) ,v))))
                       ,@(last bodies 2)))))))

(defmacro if-let (bindings &body bodies)
  "Creates new symbol bindings, and conditionally executes either
either the second-last or last form of BODIES. the last form defaults to NIL.

BINDINGS must be either single binding of the form:

 (symbol initial-form)

or a list of bindings of the form:

 ((symbol-1 initial-form-1)
  (symbol-2 initial-form-2)
  ...
  (symbol-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the symbol are bound to the corresponding values.

Any declarations can come below the bindings form, before the start of any significant code in BODIES.

If all the variables are true, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       ,@(append (butlast bodies 2) 
                 `((if (and ,@variables)
                       ,@(last bodies 2)))))))

(defmacro when-let (bindings &body body)
  "Creates new symbol bindings, and conditionally executes BODY.

BINDINGS must be either single binding of the form:

 (symbol initial-form)

or a list of bindings of the form:

 ((symbol-1 initial-form-1)
  (symbol-2 initial-form-2)
  ...
  (symbol-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the symbols are bound to the corresponding values.

Any declarations can come below the bindings form, before the start of any significant code in BODIES.

If all the variables are true, then forms in BODY are executed as an
implicit PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       ,@(remove nil
                 ;; try and retain declarations.
                 ;; if something doesn't work, try a locally-declaim or something wrapping the block instead
                 (loop for forms on body
                       if (and (listp forms)
                               (listp (car forms))
                               (or (eq 'declare (caar forms))
                                   (eq 'locally (caar forms))))
                         collect (car forms)
                       else collect `(when (and ,@variables)
                                       ,@forms)
                            and do (loop-finish))))))

(defmacro when-let* (bindings &body body) 
  "Creates new symbol bindings, and conditionally executes BODY.

BINDINGS must be either single binding of the form:

 (symbol initial-form)

or a list of bindings of the form:

 ((symbol-1 initial-form-1)
  (symbol-2 initial-form-2)
  ...
  (symbol-n initial-form-n))

Each INITIAL-FORM is executed in turn, and the symbol bound to the
corresponding value. INITIAL-FORM expressions can refer to variables
previously bound by the WHEN-LET*.

Any declarations can come below the bindings form, before the start of any significant code in BODIES.

Execution of WHEN-LET* stops immediately if any INITIAL-FORM evaluates to NIL.
If all INITIAL-FORMs evaluate to true, then BODY is executed as an implicit
PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let* ,binding-list
       ,@(remove nil
                 ;; try and retain declarations.
                 ;; if something doesn't work, try a locally-declaim or something wrapping the block instead
                 (loop for forms on body
                       if (and (listp forms)
                               (listp (car forms))
                               (or (eq 'declare (caar forms))
                                   (eq 'locally (caar forms))))
                         collect (car forms)
                       else collect `(when (and ,@variables)
                                       ,@forms)
                            and do (loop-finish))))))

