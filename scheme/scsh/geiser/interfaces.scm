;;; interfaces.scm -- define module exports

;; Copyright (C) 2013, 2014 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define-interface geiser-modules-interface
  (export ge:module-name?
	  (ge:symbol-module :syntax)
	  ge:module-location
	  ge:find-module
	  ge:module-path
	  ge:submodules
	  ge:resolve-module
	  ge:all-modules))

(define-interface geiser-utils-interface
  (export ge:make-location
	  ge:symbol->object
	  ge:pair->list
	  ge:sort-symbols!
	  ge:make-symbol-sort
	  ge:gensym?
	  ge:bound?
	  string->safe-string))

(define-interface geiser-xref-interface
  (export ge:symbol-location
	  ge:generic-methods
	  ge:callers
	  ge:callees
	  ge:find-file))

(define-interface geiser-evaluation-interface
  (export ge:eval
	  ge:macroexpand
	  ge:load-file
	  ge:set-warnings
	  (ge:disassemble :syntax)
	  ge:add-to-load-path))

(define-interface geiser-emacs-interface
  (export))

(define-interface geiser-doc-interface
  (export ge:autodoc
	  ge:module-exports
	  ge:object-signature
	  ge:symbol-documentation))

(define-interface geiser-completion-interface
  (export ge:completions 
	  ge:module-completions))

;; interfaces.scm ends here
