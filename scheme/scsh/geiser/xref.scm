;;; xref.scm -- cross-referencing utilities

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; SUCCESS: $10 = (("file") ("line"))
;; FAIL: $11 = ()
(define (symbol-location sym)
  ;; Sym -> List
  (cond ((symbol-module sym) 
	 => module-location)
        (else (let ((obj (symbol->object sym)))
		'()))))

(define (generic-methods sym)
  (let* ((gen (symbol->object sym))
         (methods '()))
    methods))

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
