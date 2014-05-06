;;; modules.scm -- module metadata

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013, 2014 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define (ge:module-name? module-name)
  (and (symbol? module-name)
       (list module-name)))

;;++ Given a SYMBOL, and an optional argument that determines whether to
;;++ search all modules for the binding of SYMBOL, search for the
;;++ binding of SYMBOL in the current module/all modules.  Note to
;;++ self: you can probably learn from what SLIME48 does here.
(define-syntax ge:symbol-module
  (syntax-rules ()
    ((ge:symbol-module proc-name . all)
     (if (ge:bound? (quote proc-name))
	 (list (second (%describe-module-hierarchy proc-name)))
	 '()))))

(define (%describe-module-hierarchy proc-name)
  (if (closure? proc-name)
      (let ((dd (template-debug-data (closure-template proc-name))))
	(debug-data-names dd))
      '()))

;;++ The output of GE:MODULE-LOCATION should be either:
;;++ '(("file") ("line")), or '() for no location.
;;++ This output format is constructed with GE:MAKE-LOCATION like so:
;;++ (ge:make-location (ge:module-path name) #f)
(define (ge:module-location name)
  '())

(define (ge:find-module mod-name)
  (and (ge:module-name? mod-name)
       #f))

(define (ge:module-path module-name)
  ;; List -> String OR #f
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

;; modules.scm ends here
