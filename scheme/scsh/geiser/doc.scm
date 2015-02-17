;;; doc.scm -- procedures providing documentation on scheme objects

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013, 2014 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define (signature id args-list)
  (let ((value (ge:symbol->object id)))
    (if (and (list? value)
	     (not (null? value)))
	(let ((args (second `(quote ,(first value))))
	      (proc (second value)))
	  (cons proc
		(list (cons "args" `(("required" ,args)
				     ("optional")
				     ("key"))))))
	`(("args") ("value" . ,value)))))

(define (obj-args obj)
  (cond ((not obj) #f)
        ((procedure? obj)
         (cond ((procedure-property obj geiser-args-key))
               ((arguments obj) =>
                (lambda (args)
                  (set-procedure-property! obj geiser-args-key args)
                  args))
               (else #f)))
        ((syntax? obj) default-macro-args)
        (else 'variable)))

(define (ge:object-signature name obj)	
  (let ((args (obj-args obj)))
    (and args (signature name args))))

(define (%autodoc id)
  (let ((args (obj-args (ge:symbol->object id))))
    (and args
	 `(,@(signature id args)
	   ("module" . ,(ge:symbol-module id))))))

(define (ge:autodoc ids)
  (if (not (list? ids))
      '()
      (map (lambda (id) (or (%autodoc id) (list id))) ids)))

(define %random (make-random 314159))	

(define (random n)			
    (modulo (%random) n))

(define (gensym str)			
  (let ((num (number->string (random 6000))))
    (string->symbol (string-append str num))))

(define geiser-args-key (gensym "geiser-args-key-")) 

;; We keep a hash table whose keys are procedure names and whose
;; values are alists with their own keys and values.
(define *procedure-properties-table* (make-table)) 

(define (set-procedure-property! obj key args) 
  ;; Object Symbol List -> Undef
  (table-set! *procedure-properties-table*
	      obj (list (cons key args))))

(define (procedure-property obj key)	
  ;; Procedure Symbol -> Symbol OR #f
  (let* ((table-val (table-ref *procedure-properties-table* obj))
	 (key-val (assoc key table-val)))
    (if key-val
	(cdr key-val)
	#f)))

(define (value-str obj)
  (format #f "~A" obj))

(define (assq-ref alist key)		
  (let ((retval (assq key alist)))
    (if retval
	(cdr retval)
	retval)))

(define default-macro-args '(((required ...)))) 

(define (procedure-source proc)
  #f)

(define (doc->args proc)
  '())

(define (arguments proc)
  ;; Procedure -> List OR #f
  (define (clist f) (lambda (x) (let ((y (f x))) (and y (list y)))))
  (cond ((doc->args proc) => list)
        ((procedure-property proc 'arglist) => (clist arglist->args))
        ((procedure-source proc) => (clist source->args))
        ((procedure-property proc 'arity) => (clist arity->args))
        (else #f)))

;;++ This procedure will come in handy once we can actually get hold
;;++ of the source of Scsh procedures. Not yet, though.
(define (source->args src)
  (if src
      (let ((formals (cadr src)))
	(cond ((list? formals) `((required . ,formals)))
	      ((pair? formals)
	       `((required . ,(car formals)) (rest . ,(cdr formals))))
	      (else #f)))
      '()))

(define (arity->args art)
  (define (gen-arg-names count)
    (map (lambda (x) '_) (iota (max count 0))))
  (if art
      (let ((req (car art))
	    (opt (cadr art))
	    (rest (caddr art)))
	`(,@(if (> req 0)
		(list (cons 'required (gen-arg-names req)))
		'())
	  ,@(if (> opt 0)
		(list (cons 'optional (gen-arg-names opt)))
		'())
	  ,@(if rest (list (cons 'rest 'rest)) '())))
      '()))

(define (arglist->args arglist)		
  (if arglist
      `((required . ,(car arglist))
	(optional . ,(cadr arglist))
	(keyword . ,(caddr arglist))
	(rest . ,(car (cddddr arglist))))
      '()))

(define (obj-signature sym obj)
  (let ((args (obj-args obj)))
    (and args (signature sym args))))

(define (ge:symbol-documentation sym)	
  (let ((obj (ge:symbol->object sym)))
    (if obj
        `(("signature" . ,(or (obj-signature sym obj) sym))
          ("docstring" . ,(docstring sym obj))))))

(define (object-documentation obj)	
  #f)

(define (docstring sym obj)
  (define (valuable?)
    (not (or (syntax? obj) (procedure? obj))))
  (call-with-string-output-port
    (lambda (port)
      (let* ((type (cond ((syntax? obj) "A macro")
                         ((procedure? obj) "A procedure")
                         (else "An object")))
             (modname (ge:symbol-module sym))
             (doc (object-documentation obj)))
        (display type port)
        (if modname
            (begin
              (display " in module " port)
              (display modname port)
              (display "." port)))
        (newline)
        (if doc (begin (newline) (display doc port)))
        (if (valuable?) (begin (newline)
                               (display "Value:" port)
                               (newline)
                               (display "   " port)
                               (display (value-str obj) port)))))))

(define (ge:module-exports mod-name)
  ;; List -> List
  (list '("modules")
	'("procs")
	'("syntax")
	'("vars")))

;; doc.scm ends here
