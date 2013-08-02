;;; modules.scm -- module metadata

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Mar 02, 2009 02:00

;; Note: I've given these procedures dummy implementations for now,
;; pending a better understanding of scsh/Scheme 48's module system.

(define-structure geiser-modules
  (export 
   module-name? 			; Check.
   symbol-module			; Dummy implementation, for now.
   module-location			; "
   find-module				; "
   module-path				; "
   submodules				; "
   all-modules				; "
   )
  (open
   scheme
   srfi-1
   re-old-funs
   geiser-utils)
(begin

(define (module-name? module-name)
  (and (symbol? module-name)
       (list module-name)))

;;; FIXME: Implement this, plz.
;; Symbol . ??? -> List
(define (symbol-module sym . all)
  '())

;; Given a SYMBOL, and an optional argument that determines whether
;; to search all modules for the binding of SYMBOL, search for the
;; binding of SYMBOL in the current module/all modules.

;; (and sym
;;      (catch 'module-name
;;        (lambda ()
;;          (apropos-fold (lambda (module name var init)
;;                          (if (eq? name sym)
;;                              (throw 'module-name (module-name module))
;;                              init))
;;                        #f
;;                        (regexp-quote (symbol->string sym))
;;                        (if (or (null? all) (not (car all)))
;;                            (apropos-fold-accessible (current-module))
;;                            apropos-fold-all)))
;;        (lambda (key . args)
;;          (and (eq? key 'module-name) (car args))))))

(define (module-location name)
  '())

;; SUCCESS: $10 = (("file") ("line"))
;; FAIL: $11 = ()
;;(make-location (module-path name) #f))

;; FIXME: Need to find an analog to Guile's `resolve-module'
(define (find-module mod-name)
  (and (module-name? mod-name)
       #f))

;; resolve-module returns an opaque directory ``thing'', or #f
;; $5 = #<directory (ice-9 session) 101237cf0>
;; (resolve-module mod-name #f ensure #f)))

;; List -> #f or String
(define (module-path module-name)
  #f)

;; (and (module-name? module-name)
;;      (or ((@@ (ice-9 session) module-filename) module-name)
;;          (module-filename (resolve-module module-name #f)))))

;; Success from `module-path' looks like: $9 = "ice-9/session.scm".
;; At first glance, it looks like ,where doesn't really work,
;; perhaps we need to turn on debugging during initial compilation?

;; List -> List
(define (submodules mod)
  '())

;;(hash-map->list (lambda (k v) v) (module-submodules mod)))

(define (root-modules)
  (submodules (resolve-module '() #f)))

(define (resolve-module name autoload version ensure)
  #f)

;; -> List
(define (all-modules)
  (cons "(scsh)" '()))

;; (define (maybe-name m)
;;   (and (module-kind m) (format #f "~A" (module-name m))))
;; (let* ((guile (resolve-module '(guile)))
;;        (roots (remove (lambda (m) (eq? m guile)) (root-modules)))
;;        (children (append-map all-child-modules roots)))
;;   (cons "(scsh)" (filter-map maybe-name children))))

;; (define* (all-child-modules mod #:optional (seen '()))
;;   (let ((cs (filter (lambda (m) (not (member m seen))) (submodules mod))))
;;     (fold (lambda (m all) (append (all-child-modules m all) all))
;;           (list mod)
;;           cs)))

))
