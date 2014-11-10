;;; evaluation.scm -- evaluation, compilation and macro-expansion

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013, 2014 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;;++ We need to determine how best to remove all of the
;;++ compilation-related code, as it has no meaning in S48-world.

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

;;++ We need EVAL-WITH-OUTPUT because of the weird way scsh (Scheme 48,
;;++ really) generates output that can break its own reader (e.g.,
;;++ ``#{Unspecific}'').  We also need to write our own version of
;;++ Guile's WITH-OUTPUT-TO-STRING (See EXPAND* below for the reason
;;++ why).

(define (eval-with-output form module)
  ;; Object Object -> String
  (let ((the-port (make-string-output-port))
	(the-string #f))
    (begin (write (eval form module) the-port)
	   (set! the-string (string-output-port-output the-port))
	   the-string)))

;;++ Output of this procedure should be in the format:
;;++ '((result "31") (output . ""))
(define (ge:eval form module-name)
  (let* ((module (or
		  (ge:find-module module-name)
		  (interaction-environment))))
    (receive (first second)
	(values
	 (eval-with-output form module)
	 '())
    (write-result first second))))

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
  (and (file-directory? dir)
       (not (member dir (lib-dirs)))
       (begin (lib-dirs-append! dir))
              #t))

(define-syntax ge:disassemble
  (syntax-rules ()
    ((ge:disassemble obj)
     (if (procedure? obj)
	 (disassemble obj)
	 (disassemble (lambda () obj))))))

;; evaluation.scm ends here
