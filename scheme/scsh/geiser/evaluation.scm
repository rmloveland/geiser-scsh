;;; evaluation.scm -- evaluation, compilation and macro-expansion

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

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
  (write (list (cons 'result (list result)) (cons 'output output)))
  (newline))

;; We need EVAL-WITH-OUTPUT because of the weird way Scsh (Scheme 48,
;; really) generates output that can break its own reader (e.g.,
;; ``#{Unspecific}'').  Note: We sorely need to write our own version
;; of Guile's WITH-OUTPUT-TO-STRING (See EXPAND* below for the reason
;; why).
(define (eval-with-output form module)
  ;; Object Object -> String
  (let ((the-port (make-string-output-port))
	(the-string #f))
    (begin (write (eval form module) the-port)
	   (set! the-string (string-output-port-output the-port))
	   the-string)))

;; Output of this procedure should be in the following format:
;; '((result "31") (output . ""))
(define (ge:eval form module-name)
  (let* ((module (or
		  (ge:find-module module-name)
		  (interaction-environment))))
    (call-with-values
	(lambda ()
	  (values (eval-with-output form module)
		  '()))
      (lambda (first second)
	(write-result first second)))))

(define ge:load-file load)

(define (expand* form . all)
  (let* ((env (package->environment (environment-for-commands)))
	 (output (schemify (expand-form (or form (car form)) env) env))
	 (the-port (make-string-output-port))
	 (the-string #f))
    (begin (write output the-port)
	   (set! the-string (string-output-port-output the-port))
	   the-string)))

(define ge:macroexpand expand*)

(define (ge:add-to-load-path dir)
  #t)
