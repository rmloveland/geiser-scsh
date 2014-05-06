;;; xref.scm -- cross-referencing utilities

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013, 2014 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;;++ This procedure outputs '(("file") ("line")) on success, '()
;;++ otherwise.
(define (ge:symbol-location sym)
  ;; Symbol -> List
  (cond ((ge:symbol-module sym) 
	 => ge:module-location)
        (else (let ((obj (ge:symbol->object sym)))
		'()))))

(define (ge:generic-methods sym)
  (let* ((gen (ge:symbol->object sym))
         (methods '()))
    methods))

(define (make-xref proc name module)
  (and proc
       `(("location" . ,(or (program-location proc) (ge:symbol-location name)))
         ("signature" . ,(ge:object-signature name proc))
         ("module" . ,(or module '())))))

(define (program-location p)
  #f)

(define (program-path p)
  #f)

(define (procedure-xref proc . mod-name)
  (let* ((proc-name (or (procedure-name proc) '<anonymous>))
         (mod-name (if (null? mod-name)
                       (ge:symbol-module proc-name)
                       (car mod-name))))
    (make-xref proc proc-name mod-name)))

(define (procedure-name sym)
  sym)
  
(define (ge:callers sym)
  (let ((mod (ge:symbol-module sym #t)))
    (and mod
         (apply append (map (lambda (procs)
                              (map (lambda (proc)
                                     (procedure-xref proc (car procs)))
                                   (cdr procs)))
                            (procedure-callers (cons mod sym)))))))

(define (procedure-callers var)
  '())

(define (ge:callees sym)
  (let ((obj (ge:symbol->object sym)))
    (and obj
         (map procedure-xref (procedure-callees obj)))))

(define (procedure-callees)
  '())

(define (ge:find-file path)
  (let loop ((dirs (lib-dirs)))
    (if (null? dirs)
	#f
        (let ((candidate (string-append (car dirs) "/" path)))
          (if (file-exists? candidate)
	      candidate
	      (loop (cdr dirs)))))))

;; xref.scm ends here
