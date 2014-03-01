;; Structures are listed in dependency order.

(define-structure geiser-utils geiser-utils-interface
  (open scheme 
	big-util
	formats
	sort
	packages
	command-processor)
  (files utils))

(define-structure geiser-modules geiser-modules-interface
  (open scheme 

	disclosers
	closures
	geiser-utils)
  (files modules))

(define-structure geiser-evaluation geiser-evaluation-interface
  (open scheme 
	
	command-processor
	extended-ports
	environments
	packages
	syntactic
	geiser-modules
	nodes
	pp)
  (files evaluation))

(define-structure geiser-doc geiser-doc-interface
  (open scheme 
	formats
	geiser-utils
	geiser-modules
	syntactic
	pp
	random
	extended-ports
	tables)
  (files doc))

(define-structure geiser-completion geiser-completion-interface
  (open scheme 
	packages
	packages-internal
	handle				; ignore-errors
	util 				; fold
	sort
	tables
	extended-ports
	formats
	geiser-utils
	geiser-modules)
  (files completion))

(define-structure geiser-xref geiser-xref-interface
  (open scheme 
	geiser-utils
	geiser-modules
	geiser-doc)
  (files xref))

(define-structure geiser-emacs geiser-emacs-interface
  (open scheme 
	geiser-evaluation
	command-processor
	geiser-modules
	geiser-completion
	geiser-xref
	geiser-doc)
  (files emacs))
