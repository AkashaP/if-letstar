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
         (variables (mapcar #'car binding-list))
         (decls (if (or (eq 'declare (caar bodies))
                        (eq 'locally (caar bodies)))
                    (car bodies)))
         (body (remove decls bodies)))
    `(let ,binding-list
       ,@(append (if decls `(,decls)) 
                 `((if (and ,@variables)
                       ,(car body)
                       ,@(if (cdr on-fail)
                             `((let ((it (remove nil
                                                 `(,,@(loop for var in variables
                                                            collect `(and (not ,var) ',var))))))
                                 ,@(cdr on-fail)
                                 ,(cadr body)))
                             (cdr body))))))))

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
         (variables (mapcar #'car binding-list))
         (decls (if (or (eq 'declare (caar body))
                        (eq 'locally (caar body)))
                    (car body)))
         (body (remove decls body)))
    `(let ,binding-list
       ,@(if decls `(,decls))
       ,(if (not (cdr on-fail))
            `(when (and ,@variables)
               ,@body)
            `(if (and ,@variables)
                 (progn ,@body)
                 (let ((it (remove nil
                                   `(,,@(loop for var in variables
                                              collect `(and (not ,var) ',var))))))
                   ,@(cdr on-fail)
                   nil))))))

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
         (variables (mapcar #'car binding-list))
         (fake-variables (loop repeat (length variables)
                               for (s v) in binding-list
                               collect `(,s ,v)))
         (fake-to-bindings (loop for (f v1) in fake-variables
                                 for (s v2) in binding-list
                                 collect `(,s ,f)))
         )
    `(let ,(mapcar 'car fake-variables)
       ,(if binding-list
            (labels ((bind (b)
                       `(if (setq ,@(car b))
                            ,(if (cdr b)
                                 `(,@(bind (cdr b)))
                                 `(let* ,fake-to-bindings
                                    ,@body))
                            ,@(when (cdr on-fail)
                                `((let ((it ',(caar b))) 
                                    ,@(cdr on-fail)))))))
              (bind fake-variables))))))

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
         (variables (mapcar #'car binding-list)))
    `(let ,variables
       ,@(loop for forms on bodies
               while (and (listp forms)
                          (listp (car forms))
                          (or (eq 'declare (caar forms))
                              (eq 'locally (caar forms))))
               if (car forms) 
                 collect (car forms) into result
               finally (return `(,@result (if ,(if binding-list
                                                   (labels ((bind (b)
                                                              `(if (setq
                                                                    ,@(car b))
                                                                   ,@(if (cdr b)
                                                                         `(,(bind (cdr b)))
                                                                         `(,(caar b)))
                                                                   ,@(when (cdr on-fail)
                                                                       `((let ((it ',(caar b))) 
                                                                           ,@(cdr on-fail)))))))
                                                     (bind binding-list)))
                                              ,@forms)))))))
