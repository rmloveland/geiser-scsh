;;; modules.scm -- module metadata

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define-structure geiser-modules
  (export 
   module-name?
   symbol-module
   module-location
   find-module
   module-path
   submodules
   all-modules)
  (open
   scheme
   srfi-1
   re-old-funs
   geiser-utils)
(begin

(define (module-name? module-name)
  (and (symbol? module-name)
       (list module-name)))

;; Given a SYMBOL, and an optional argument that determines whether to
;; search all modules for the binding of SYMBOL, search for the
;; binding of SYMBOL in the current module/all modules.
(define (symbol-module sym . all)
  ;; Symbol . ? -> List
  (let ((val (symbol->object sym)))
    (if val
	(list (vector-ref val 1))
	'())))

;; SUCCESS: $10 = (("file") ("line"))
;; FAIL: $11 = ()
;; (make-location (module-path name) #f)
(define (module-location name)
  '())

;; FIXME: Need to find an analog to Guile's `resolve-module'
;; procedure, which returns an opaque directory ``thing'',
;; or #f, e.g.:
;; $5 = #<directory (ice-9 session) 101237cf0> (resolve-module mod-name #f ensure #f)))
(define (find-module mod-name)
  (and (module-name? mod-name)
       #f))

(define (module-path module-name)
  ;; List -> String or #f
  #f)

(define (submodules mod)
  ;; List -> List
  '())

(define (root-modules)
  (submodules (resolve-module '() #f)))

(define (resolve-module name autoload version ensure)
  #f)


(define (all-modules)
  ;; -> List
  (cons "(scsh)" '()))

))
