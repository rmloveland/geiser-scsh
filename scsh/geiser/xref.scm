;;; xref.scm -- cross-referencing utilities

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define-structure geiser-xref
  (export 
   symbol-location
   generic-methods
   callers
   callees
   find-file)
  (open 
   geiser-utils
   geiser-modules
   geiser-doc
   )

(begin

;; SUCCESS: $10 = (("file") ("line"))
;; FAIL: $11 = ()
(define (symbol-location sym)
  ;; Sym -> List
  (cond ((symbol-module sym) => module-location)
        (else (let ((obj (symbol->object sym)))
		'()))))

(define (generic-methods sym)
  (let* ((gen (symbol->object sym))
         (methods
	  '()))))

(define (make-xref proc name module)
  (and proc
       `(("location" . ,(or (program-location proc) (symbol-location name)))
         ("signature" . ,(object-signature name proc))
         ("module" . ,(or module '())))))

(define (program-location p)
  #f)

(define (program-path p)
  #f)

(define (procedure-xref proc . mod-name)
  (let* ((proc-name (or (procedure-name proc) '<anonymous>))
         (mod-name (if (null? mod-name)
                       (symbol-module proc-name)
                       (car mod-name))))
    (make-xref proc proc-name mod-name)))

(define (procedure-name sym)
  sym)
  
(define (callers sym)
  (let ((mod (symbol-module sym #t)))
    (and mod
         (apply append (map (lambda (procs)
                              (map (lambda (proc)
                                     (procedure-xref proc (car procs)))
                                   (cdr procs)))
                            (procedure-callers (cons mod sym)))))))

  ;; Returns a list in this format:
  ;; (((guile) #<procedure for-each (f l) | (f l1 l2) | (f l1 . rest)> #<procedure map (f l) | (f l1 l2) | (f l1 . rest)>) ((ice-9 session) #<procedure help-doc (term regexp)>) ((ice-9 threads) #<procedure n-par-for-each (_ _ . _)> #<procedure n-for-each-par-map (_ _ _ . _)> #<procedure n-par-map (_ _ . _)>) ((ice-9 deprecated) #<procedure transform-usage-lambda (cases)>) ((system vm program) #<procedure arguments-alist->lambda-list (arguments-alist)>) ((language tree-il compile-glil) #<procedure flatten-lambda-case (lcase allocation self self-label fix-labels emit-code)>) ((language tree-il fix-letrec) #<procedure fix-letrec! (x)>) ((language tree-il analyze) #<procedure format-string-argument-count (fmt)> #<procedure validate-arity (proc application lexical?)>) ((language tree-il) #<procedure tree-il->scheme (e)>) ((oop goops util) #<procedure for-each* (fn . l)> #<procedure map* (fn . l)>) ((oop goops dispatch) #<procedure compute-dispatch-procedure (gf cache)>) ((srfi srfi-1) #<procedure for-each (f l) | (f l1 . rest)> #<procedure any (pred ls . lists)> #<procedure filter-map (proc list1 . rest)> #<procedure every (pred ls . lists)> #<procedure zip (clist1 . rest)> #<procedure fold (kons knil list1 . rest)> #<procedure map (f l) | (f l1 . rest)> #<procedure list-index (pred clist1 . rest)> #<procedure fold-right (kons knil clist1 . rest)>) ((geiser doc) #<procedure module-exports (mod-name)>))
(define (procedure-callers var)
  '())

(define (callees sym)
  (let ((obj (symbol->object sym)))
    (and obj
         (map procedure-xref (procedure-callees obj)))))

(define (procedure-callees)
  '())

(define (find-file path)
  (let loop ((dirs (lib-dirs)))
    (if (null? dirs)
	#f
        (let ((candidate (string-append (car dirs) "/" path)))
          (if (file-exists? candidate)
	      candidate
	      (loop (cdr dirs)))))))

))
