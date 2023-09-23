(defpackage #:if-letstar
  (:nicknames #:if-let*)
  (:use #:cl)
  (:export #:if-let*
           #:if-let
           #:ifn-let
           #:when-let
           #:when-let*
           #:on-fail
           #:it))

(in-package :if-letstar)

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
  (let* ((on-fail (find 'on-fail bindings :key 'car))
         (bindings (remove on-fail bindings))
         (binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    (multiple-value-bind (decls body)
        (loop for b in bodies
              if (and (listp b)
                      (or (eq 'declare (car b))
                          (eq 'locally (car b))))
                collect b into decls
              else collect b into body
              finally (return (values decls body)))
      `(let ,binding-list
         ,@decls 
         (if (and ,@variables)
             ,(car body)
             ,@(if (cdr on-fail)
                   `((let ((it (remove nil
                                       `(,,@(loop for var in variables
                                                  collect `(and (not ,var) ',var))))))
                       ,@(cdr on-fail)
                       ,(cadr body)))
                   (cdr body)))))))

(defmacro ifn-let (bindings else then)
  "IF-LET, but the else clause comes first."
  `(if-let ,bindings
     ,then
     ,else))

(defmacro when-let (bindings &body body)
  "Creates new symbol bindings, and conditionally executes BODY.

([] means docs from alexandria:)
[BINDINGS must be either single binding of the form:

 (symbol initial-form)

or a list of bindings of the form:

 ((symbol-1 initial-form-1)
  (symbol-2 initial-form-2)
  ...
  (symbol-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the symbols are bound to the corresponding values.]

(not from alexandria:)
Any declarations can come below the bindings form, before the start of any significant code in BODIES.

If all the variables are true, then forms in BODY are executed as an
implicit PROGN."
  (let* ((on-fail (find 'on-fail bindings :key 'car))
         (bindings (remove on-fail bindings))
         (binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    (multiple-value-bind (decls body)
        (loop for b in body
              if (and (listp b)
                      (or (eq 'declare (car b))
                          (eq 'locally (car b))))
                collect b into decls
              else collect b into body
              finally (return (values decls body)))
      `(let ,binding-list
         ,@decls
         ,(if (not (cdr on-fail))
              `(when (and ,@variables)
                 ,@body)
              `(if (and ,@variables)
                   (progn ,@body)
                   (let ((it (remove nil
                                     `(,,@(loop for var in variables
                                                collect `(and (not ,var) ',var))))))
                     ,@(cdr on-fail)
                     nil)))))))

(defmacro when-let* (bindings &body body) 
  "Creates new symbol bindings, and conditionally executes BODY.

([] means docs from alexandria:)
[BINDINGS must be either single binding of the form:

 (symbol initial-form)

or a list of bindings of the form:

 ((symbol-1 initial-form-1)
  (symbol-2 initial-form-2)
  ...
  (symbol-n initial-form-n))

Each INITIAL-FORM is executed in turn, and the symbol bound to the
corresponding value. INITIAL-FORM expressions can refer to variables
previously bound by the WHEN-LET*.]

(not from alexandria:)
Additionally, (if-letstar:on-fail (break)) in bindings can be used to splice code on failure.

Any declarations can come below the bindings form, before the start of any significant code in BODIES.

[Execution of WHEN-LET* stops immediately if any INITIAL-FORM evaluates to NIL.
If all INITIAL-FORMs evaluate to true, then BODY is executed as an implicit
PROGN.]"



  
  (let* ((on-fail (find 'on-fail bindings :key 'car))
         (bindings (remove on-fail bindings))
         (binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (values-list (mapcar #'cadr binding-list))
         (target-variables (mapcar #'car binding-list))
         (fake-vars (loop repeat (length bindings) collect (gensym)))
         (fake-to-bindings (loop for f in fake-vars
                                 for (s v2) in binding-list
                                 collect `(,s ,f)))
         (fake-variables (loop repeat (length bindings)
                               for s in fake-vars
                               for v in values-list
                               collect `(,s ,v))))
    (multiple-value-bind (decls body)
        (loop for b in body
              if (and (listp b)
                      (or (eq 'declare (car b))
                          (eq 'locally (car b))))
                collect b into decls
              else collect b into body
              finally (return (values decls body)))
      `(let ,(mapcar 'cadr fake-to-bindings)
         ,(if binding-list
              (labels ((bind (b tgt)
                         `(if (setq ,@(car b))
                              ,(if (cdr b)
                                   `(let ((,(car tgt) ,(caar b)))
                                      ,(bind (cdr b) (cdr tgt)))
                                   `(let* ,fake-to-bindings
                                      ,@decls
                                      ,@body))
                              ,@(when (cdr on-fail)
                                  `((let ((it ',(caar b))) 
                                      ,@(cdr on-fail)))))))
                (bind fake-variables target-variables)))))))

(defmacro if-let* (bindings &body bodies)
  "Creates new symbol bindings, and conditionally executes either the second-last or last form of BODIES.

([] means docs from alexandria:)
[BINDINGS must be either single binding of the form:

 (symbol initial-form)

or a list of bindings of the form:

 ((symbol-1 initial-form-1)
  (symbol-2 initial-form-2)
  ...
  (symbol-n initial-form-n))]

(not from alexandria:)
Additionally, (if-letstar:on-fail (break)) in bindings can be used to splice code on failure.

Any declarations can come below the bindings form, before the start of any significant code in BODIES.
Each INITIAL-FORM is executed in turn, and the variable bound to the
corresponding value. INITIAL-FORM expressions can refer to symbols
previously bound by the IF-LET*.

Execution of IF-LET* causes the last form of BODIES to evaluate if any INITIAL-FORM evaluates to NIL.
If all INITIAL-FORMs evaluate to true, then the second-last form of BODIES is executed."
  (let* ((on-fail (find 'on-fail bindings :key 'car))
         (bindings (remove on-fail bindings))
         (binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (values-list (mapcar #'cadr binding-list))
         (target-variables (mapcar #'car binding-list))
         (fake-vars (loop repeat (length bindings) collect (gensym)))
         (fake-variables (loop repeat (length bindings)
                               for s in fake-vars
                               for v in values-list
                               collect `(,s ,v)))
         (true-to-fake (mapcar 'list target-variables fake-vars)))
    (multiple-value-bind (decls body)
        (loop for b in bodies
              if (and (listp b)
                      (or (eq 'declare (car b))
                          (eq 'locally (car b))))
                collect b into decls
              else collect b into body
              finally (return (values decls body)))
      `(let ,fake-vars
         (if ,(if binding-list
                  (labels ((bind (b tgt)
                             `(if (setq ,@(car b))
                                  ,(if (cdr b)
                                       `(let ((,(car tgt) ,(caar b)))
                                          ,(bind (cdr b) (cdr tgt)))
                                       t)
                                  ,@(when (cdr on-fail)
                                      `((let ((it ',(caar b))) 
                                          ,@(cdr on-fail)))))))
                    (bind fake-variables target-variables)))
             ,@(loop for body in body
                     collect `(let ,true-to-fake ,@decls ,body)))))))
