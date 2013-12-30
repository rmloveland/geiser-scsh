(define-interface geiser-modules-interface
  (export module-name?
	  symbol-module
	  module-location
	  find-module
	  module-path
	  submodules
	  resolve-module
	  all-modules))

(define-interface geiser-utils-interface
  (export make-location
	  symbol->object
	  pair->list
	  sort-symbols!
	  make-symbol-sort
	  gensym?
	  bound?))

(define-interface geiser-xref-interface
  (export symbol-location
	  generic-methods
	  callers
	  callees
	  find-file))

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
	  module-exports
	  object-signature
	  symbol-documentation))

(define-interface geiser-completion-interface
  (export ge:completions 
	  ge:module-completions))
