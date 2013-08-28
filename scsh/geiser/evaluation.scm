;;; evaluation.scm -- evaluation, compilation and macro-expansion

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define-structure geiser-evaluation
  (export 
   ge:eval
   ge:macroexpand
   ge:load-file
   ge:set-warnings
   ge:add-to-load-path
   )
  (open 
   scheme
   scsh
   command-processor
   extended-ports
   environments
   packages
   syntactic
   geiser-modules
   nodes
   srfi-1
   pp
   )

(begin

(define compile-opts '())
(define compile-file-opts '())

(define default-warnings '(arity-mismatch unbound-variable format))
(define verbose-warnings `(unused-variable ,@default-warnings))

(define (object->string obj)
  (write obj))

(define (ge:set-warnings wl)
  '())

(ge:set-warnings 'none)

(define (write-result result output)
  (write (list (cons 'result result) (cons 'output output)))
  (newline))

(define (call-with-result thunk)
  (let* ((result '())
	 (output
	  (with-error-output-port (current-output-port)
				  (lambda () (set! result (cons (thunk) result))))))
    (write-result result output)))

;; Output of this procedure should be in the following format:
;; '((result "31") (output . ""))
(define (ge:eval form module-name)
  (let* ((module (or 
		  (find-module module-name)
		  (interaction-environment))))
    (receive (first second)
	(values
	 (eval form module)
	 (lambda vs (map object->string vs)))
      (write-result first second))))

(define ge:load-file load)

(define (expand* form . all)
  (let ((env (package->environment (environment-for-commands))))
    (schemify (expand-form (or (car form) form) env) env)))

(define ge:macroexpand expand*)

(define (ge:add-to-load-path dir)
  (and (file-directory? dir)
       (not (member dir (lib-dirs)))
       (begin (lib-dirs-append! dir))
              #t))

))
