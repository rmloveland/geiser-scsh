;;; modules.scm -- module metadata

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define (ge:module-name? module-name)
  (and (symbol? module-name)
       (list module-name)))

;; Given a SYMBOL, and an optional argument that determines whether to
;; search all modules for the binding of SYMBOL, search for the
;; binding of SYMBOL in the current module/all modules.  Note: you can
;; probably learn from what SLIME48 does here.
(define (ge:symbol-module sym . all)
  ;; Symbol . ? -> List
  (let ((val (ge:symbol->object sym)))
    (if val
	(list (vector-ref val 1))
	'())))

;; Example output from this procedure:
;; > (describe-module-hierarchy car)
;; '(car scheme-level-0)
;; Note that PROC-NAME must be defined or else this procedure blows
;; up.
(define (describe-module-hierarchy proc-name)
  (if (closure? proc-name)
      (let ((dd (template-debug-data (closure-template proc-name))))
	(debug-data-names dd))
      '()))

;; SUCCESS: $10 = (("file") ("line"))
;; FAIL: $11 = ()
;; (make-location (module-path name) #f)
(define (ge:module-location name)
  '())

;; Note: we need to find an analog to Guile's `resolve-module'
;; procedure, which returns an opaque directory ``thing'', or #f,
;; e.g.: 

;; $5 = #<directory (ice-9 session) 101237cf0> (resolve-module mod-name #f ensure #f)))
(define (ge:find-module mod-name)
  (and (ge:module-name? mod-name)
       #f))

(define (ge:module-path module-name)
  ;; List -> String or #f
  #f)

(define (ge:submodules mod)
  ;; List -> List
  '())

(define (ge:root-modules)
  (ge:submodules (ge:resolve-module '() #f 0 #f)))

(define (ge:resolve-module name autoload version ensure)
  #f)

(define (ge:all-modules)
  ;; -> List
  (cons "(scsh)" '()))
