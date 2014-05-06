;;; packages.scm -- define structures that back our interfaces

;; Copyright (C) 2013, 2014 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;;++ The following structures are listed in dependency order.

(define-structure geiser-utils geiser-utils-interface
  (open scheme-with-scsh
	big-util
	re-old-funs
	formats
	sorting
	packages
	command-processor)
  (files utils))

(define-structure geiser-modules geiser-modules-interface
  (open scheme-with-scsh
	srfi-1
	re-old-funs
	disclosers
	closures
	geiser-utils)
  (files modules))

(define-structure geiser-evaluation geiser-evaluation-interface
  (open scheme-with-scsh
	scsh
	command-processor
	extended-ports
	environments
	packages
	syntactic
	geiser-modules
	nodes
	srfi-1
	pp)
  (files evaluation))

(define-structure geiser-doc geiser-doc-interface
  (open scheme-with-scsh
	scsh
	geiser-utils
	geiser-modules
	srfi-1
	syntactic
	pp
	random
	tables)
  (files doc))

(define-structure geiser-completion geiser-completion-interface
  (open scheme-with-scsh
	packages
	packages-internal
	handle				; ignore-errors
	util 				; fold
	sort
	tables
	srfi-13				; string-prefix-ci?
	geiser-utils
	geiser-modules)
  (files completion))

(define-structure geiser-xref geiser-xref-interface
  (open scheme-with-scsh
	geiser-utils
	geiser-modules
	geiser-doc)
  (files xref))

(define-structure geiser-emacs geiser-emacs-interface
  (open scheme-with-scsh
	geiser-evaluation
	command-processor
	geiser-modules
	geiser-completion
	geiser-xref
	geiser-doc)
  (files emacs))

;; packages.scm ends here
