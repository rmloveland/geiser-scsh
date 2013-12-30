;;; utils.scm -- utility functions

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define (symbol->object sym)
  (and (symbol? sym)
       (bound? sym)))

(define (bound? name)
  (package-lookup (environment-for-commands) name))

(define (pair->list pair)
  (let loop ((d pair) (s '()))
    (cond ((null? d) (reverse! s))
          ((symbol? d) (reverse! (cons d s)))
          (else (loop (cdr d) (cons (car d) s))))))

(define (make-location file line)
  (list (cons "file" (if (string? file) file '()))
        (cons "line" (if (number? line) (+ 1 line) '()))))

(define (sort-symbols! syms)
  (let ((cmp (lambda (l r)
               (string<? (symbol->string l) (symbol->string r)))))
    (list-sort! syms cmp)))

(define (make-symbol-sort sel)
  (let ((cmp (lambda (a b)
               (string<? (symbol->string (sel a))
                         (symbol->string (sel b))))))
    (lambda (syms)
      (list-sort! syms cmp))))

(define (gensym? sym)
  (and (symbol? sym) (gensym-name? (format #f "~A" sym))))

(define (gensym-name? name)
  (and (string-match "^#[{]" name) #t))
