;;; completion.scm -- completing known symbols and module names

;; Copyright (C) 2009, 2012 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define-structure geiser-completion
  (export 
   completions 
   module-completions
   )
  (open
   scheme
   geiser-utils
   geiser-modules
   )

(begin

(define (completions prefix)
  '())

(define (module-completions prefix)
  '())

))
