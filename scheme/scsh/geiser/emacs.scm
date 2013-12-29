;;; emacs.scm -- procedures for emacs interaction: entry point

;; Copyright (C) 2009, 2010, 2011 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define-structure geiser-emacs
  (export '())
  (open
   scheme
   geiser-evaluation
   command-processor
   ;; TODO: Figure out how to do the below export prefixing with the
   ;; S48 module system.
   geiser-modules		  ; renamer (symbol-prefix-proc 'ge: )
   geiser-completion		  ; renamer (symbol-prefix-proc 'ge: )
   geiser-xref			  ; renamer (symbol-prefix-proc 'ge: )
   geiser-doc			 ; renamer (symbol-prefix-proc 'ge: )
   )

(begin

(define this-module (resolve-module '(geiser emacs)))

(define-command-syntax 'geiser-no-values "<>"
  "No-op command used internally by Geiser."
  ())

(define (geiser-no-values)
  (values))

(define-command-syntax 'geiser-newline "No args"
  "Used by Geiser to emit a newline."
  (newline))

(define-command-syntax 'geiser-eval "<module> <form> <args>"
  "Used by Geiser to evaluate code."
  '(module form args))

(define (geiser-eval module form args)
  (if (null? args)
      (let ((proc (eval form this-module)))
        (ge:eval `(,proc ,@args) mod))))

(define-command-syntax 'geiser-load-file "<file>"
  "Used by geiser to load files."
  '(file))

(define (geiser-load-file file)
  (ge:load-file file))

))
