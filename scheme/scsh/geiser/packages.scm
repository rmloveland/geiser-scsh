;; Structures are listed in dependency order.

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
	;; TODO: Figure out how to export symbols with prefixes using
	;; the S48 module system.
	geiser-modules            ; renamer (symbol-prefix-proc 'ge: )
	geiser-completion	  ; renamer (symbol-prefix-proc 'ge: )
	geiser-xref		  ; renamer (symbol-prefix-proc 'ge: )
	geiser-doc		  ; renamer (symbol-prefix-proc 'ge: )
	)
  (files emacs))
