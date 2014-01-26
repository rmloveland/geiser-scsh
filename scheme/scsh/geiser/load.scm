;; See geiser-scsh.el for the value of =geiser-scsh-dir.

(config '(load "=geiser-scsh-dir/interfaces.scm"
               "=geiser-scsh-dir/packages.scm"
	       "=geiser-scsh-dir/apropos.scm"))


;; Next, we load the geiser packages in the correct order.
(load-package 'geiser-utils)
(load-package 'geiser-modules)
(load-package 'geiser-evaluation)
(load-package 'geiser-doc)
(load-package 'geiser-completion)
(load-package 'geiser-xref)
(load-package 'geiser-emacs)
(load-package 'apropos)

(user)

(open 'geiser-utils)
(open 'geiser-modules)
(open 'geiser-evaluation)
(open 'geiser-doc)
(open 'geiser-completion)
(open 'geiser-xref)
(open 'geiser-emacs)
(open 'apropos)
