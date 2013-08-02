;;; modules.scm -- module metadata

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Mar 02, 2009 02:00

(define-structure geiser-modules
  (export 
   module-name? 			; Check.
   symbol-module
   module-location
   find-module
   module-path
   submodules
   all-modules
   )
  (open
   scheme
   srfi-1
   re-old-funs
   geiser-utils)

  ;; #:use-module (system vm program)
  ;; #:use-module (ice-9 session)
(begin

(define (module-name? module-name)
  (and (symbol? module-name)
       (list module-name)))

(define (symbol-module sym . all)
  ;; Symbol . ??? -> List
  

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
  (make-location (module-path name) #f))

;; FIXME: Need to find an analog to Guile's `resolve-module'
(define (find-module mod-name)
  (and (module-name? mod-name)
       (resolve-module mod-name #f #:ensure #f)))

(define (module-path module-name)
  (and (module-name? module-name)
       (or ((@@ (ice-9 session) module-filename) module-name)
           (module-filename (resolve-module module-name #f)))))

(define (submodules mod)
  (hash-map->list (lambda (k v) v) (module-submodules mod)))

(define (root-modules)
  (submodules (resolve-module '() #f)))

(define (all-modules)
  (define (maybe-name m)
    (and (module-kind m) (format #f "~A" (module-name m))))
  (let* ((guile (resolve-module '(guile)))
         (roots (remove (lambda (m) (eq? m guile)) (root-modules)))
         (children (append-map all-child-modules roots)))
    (cons "(guile)" (filter-map maybe-name children))))

(define* (all-child-modules mod #:optional (seen '()))
  (let ((cs (filter (lambda (m) (not (member m seen))) (submodules mod))))
    (fold (lambda (m all) (append (all-child-modules m all) all))
          (list mod)
          cs)))
