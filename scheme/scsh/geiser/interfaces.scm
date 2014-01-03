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
	  ge:bound?))

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
