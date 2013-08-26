;; geiser-scsh.el -- scsh's implementation of the geiser protocols

;; Copyright (C) 2009, 2010, 2011, 2012, 2013 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun Mar 08, 2009 23:03



(require 'geiser-connection)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser-eval)
(require 'geiser-edit)
(require 'geiser-log)
(require 'geiser)

(require 'compile)
(require 'info-look)

(eval-when-compile (require 'cl))


;;; Customization:

(defgroup geiser-scsh nil
  "Customization for Geiser's Scsh flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-scsh-binary
  (cond ((eq system-type 'windows-nt) "scsh.exe")
        ((eq system-type 'darwin) "scsh")
        (t "scsh"))
  "Name to use to call the Scsh executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-load-path nil
  "A list of paths to be added to Scsh's load path when it's
started."
  :type '(repeat file)
  :group 'geiser-scsh)

;;; I don't think scsh has an exact analogue for this
;;; functionality. Instead you specify startup files in the script
;;; header itself.

(geiser-custom--defcustom geiser-scsh-init-file "~/.scsh-geiser"
  "Initialization file with user code for the Scsh REPL.
If all you want is to load ~/.scsh, set
`geiser-scsh-load-init-file-p' instead."
  :type 'string
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-load-init-file-p nil
  "Whether to load ~/.scsh when starting Scsh.
Note that, due to peculiarities in the way Scsh loads its init
file, using `geiser-scsh-init-file' is not equivalent to setting
this variable to t."
  :type 'boolean
  :group 'geiser-scsh)

;;; Also not relevant for scsh.

(geiser-custom--defcustom geiser-scsh-debug-show-bt-p nil
  "Whether to autmatically show a full backtrace when entering the debugger.
If `nil', only the last frame is shown."
  :type 'boolean
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-jump-on-debug-p nil
  "Whether to autmatically jump to error when entering the debugger.
If `t', Geiser will use `next-error' to jump to the error's location."
  :type 'boolean
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-show-debug-help-p nil
  "Whether to show brief help in the echo area when entering the debugger."
  :type 'boolean
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-warning-level 'none
  "Verbosity of the warnings reported by Scsh.

You can either choose one of the predefined warning sets, or
provide a list of symbols identifying the ones you want. Possible
choices are arity-mismatch, unbound-variable, unused-variable and
unused-toplevel. Unrecognised symbols are ignored.

The predefined levels are:

  - Medium: arity-mismatch, unbound-variable, format
  - High: arity-mismatch, unbound-variable, unused-variable, format
  - None: no warnings

Changes to the value of this variable will automatically take
effect on new REPLs. For existing ones, use the command
\\[geiser-scsh-update-warning-level]."
  :type '(choice (const :tag "Medium (arity and unbound vars)" medium)
                 (const :tag "High (also unused vars)" high)
                 (const :tag "No warnings" none)
                 (repeat :tag "Custom" symbol))
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-extra-keywords nil
  "Extra keywords highlighted in Scsh scheme buffers."
  :type '(repeat string)
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-case-sensitive-p t
  "Non-nil means keyword highlighting is case-sensitive."
  :type 'boolean
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-manual-lookup-other-window-p nil
  "Non-nil means pop up the Info buffer in another window."
  :type 'boolean
  :group 'geiser-scsh)

(geiser-custom--defcustom geiser-scsh-manual-lookup-nodes
                          '("scsh")
  "List of info nodes that, when present, are used for manual lookups"
  :type '(repeat string)
  :group 'geiser-scsh)


;;; REPL support:

(defun geiser-scsh--binary ()
  (if (listp geiser-scsh-binary)
      (car geiser-scsh-binary)
    geiser-scsh-binary))

(defun geiser-scsh--parameters ()
  "Return a list with all parameters needed to start Scsh.
This function uses `geiser-scsh-init-file' if it exists."
  '())
  ;; (let ((init-file (and (stringp geiser-scsh-init-file)
  ;;                       (expand-file-name geiser-scsh-init-file)))
  ;;       (q-flags (and (not geiser-scsh-load-init-file-p) '("-q"))))
  ;; `(,@(and (listp geiser-scsh-binary) (cdr geiser-scsh-binary))
  ;;   ,@q-flags "-L" ,(expand-file-name "scsh/" geiser-scheme-dir)
  ;;   ,@(apply 'append (mapcar (lambda (p) (list "-L" p))
  ;;                            geiser-scsh-load-path))
  ;;   ,@(and init-file (file-readable-p init-file) (list "-l" init-file)))))

(defvar geiser-scsh--prompt-regexp "^[0-9]?> ")
(defvar geiser-scsh--debugger-prompt-regexp "^inspect: ")


;;; Evaluation support:
(defsubst geiser-scsh--linearize-args (args)
  (mapconcat 'identity args " "))

;; FIXME: Update the below to use the appropriate Scsh command
;; processor commands. A first guess at each is in the comments.
(defun geiser-scsh--geiser-procedure (proc &rest args)
  (case proc
    ((eval compile) (format ",run %s %s%s"
                            (or (car args) "#f")
                            (geiser-scsh--linearize-args (cdr args))
                            (if (cddr args) "" " ()")))
    ((load-file compile-file) (format ",load %s" (car args)))
    ((no-values) "")
    (t (format "ge:%s (%s)" proc (geiser-scsh--linearize-args args)))))

(defvar geiser-scsh--module-re
  "define-structure +\\(([^)]+)\\)")

(defvar geiser-scsh--library-re
  "define-interface +\\(([^)]+)\\)")

(defun geiser-scsh--get-module (&optional module)
  (cond ((null module)
         (save-excursion
           (ignore-errors
             (while (not (zerop (geiser-syntax--nesting-level)))
               (backward-up-list)))
           (if (or (re-search-backward geiser-scsh--module-re nil t)
                   (looking-at geiser-scsh--library-re)
                   (re-search-forward geiser-scsh--module-re nil t))
               (geiser-scsh--get-module (match-string-no-properties 1))
             :f)))
        ((listp module) module)
        ((stringp module)
         (condition-case nil
             (car (geiser-syntax--read-from-string module))
           (error :f)))
        (t :f)))

(defun geiser-scsh--module-cmd (module fmt &optional def)
  (when module
    (let* ((module (geiser-scsh--get-module module))
           (module (cond ((or (null module) (eq module :f)) def)
                         (t (format "%s" module)))))
      (and module (format fmt module)))))

(defun geiser-scsh--import-command (module)
  (geiser-scsh--module-cmd module ",open %s"))

(defun geiser-scsh--enter-command (module)
  (geiser-scsh--module-cmd module ",%s" module))

(defun geiser-scsh--exit-command () ",exit")

(defun geiser-scsh--symbol-begin (module)
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^-()>") (point))))


;;; Error display

(defun geiser-scsh--enter-debugger ()
  (let ((bt-cmd (format ",condition\n,debug\n")))
    ;; (if geiser-scsh-debug-show-bt-p "bt" "fr"))))
    (compilation-forget-errors)
    (goto-char (point-max))
    (geiser-repl--prepare-send)
    (comint-send-string nil bt-cmd)
    (when geiser-scsh-show-debug-help-p
      (message "Debug REPL. Enter ,q to quit, ,h for help."))
    (when geiser-scsh-jump-on-debug-p
      (accept-process-output (get-buffer-process (current-buffer))
                             0.2 nil t)
      (ignore-errors (next-error)))))

(defun geiser-scsh--display-error (module key msg)
  (newline)
  (when (stringp msg)
    (save-excursion (insert msg))))


;;; Trying to ascertain whether a buffer is Scsh Scheme:

(defvar geiser-scsh--guess-re
  (format "\\(%s\\|#! *.+\\(/\\| \\)scsh\\( *\\\\\\)?\\)"
          geiser-scsh--module-re))

(defun geiser-scsh--guess ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward geiser-scsh--guess-re nil t)))


;;; Keywords and syntax

(defun geiser-scsh--keywords ()
  (when geiser-scsh-extra-keywords
    `((,(format "[[(]%s\\>" (regexp-opt geiser-scsh-extra-keywords 1))
       . 1))))

(geiser-syntax--scheme-indent
 (c-declare 0)
 (c-lambda 2)
 (lambda* 1)
 (pmatch defun)
 (sigaction 1)
 (with-fluid* 1)
 (with-fluids 1)
 (with-fluids* 1)
 (with-method 1))



;;; Compilation shell regexps

(defvar geiser-scsh--path-rx "^In \\([^:\n ]+\\):\n")

(defvar geiser-scsh--rel-path-rx "^In +\\([^/\n :]+\\):\n")

(defvar geiser-scsh--file-cache (make-hash-table :test 'equal))

(defun geiser-scsh--resolve-file (file)
  (when (and (stringp file)
             (not (member file '("socket" "stdin" "unknown file"))))
    (if (file-name-absolute-p file) file
      (or (gethash file geiser-scsh--file-cache)
          (puthash file
                   (geiser-eval--send/result `(:eval (:ge find-file ,file)))
                   geiser-scsh--file-cache)))))

(defun geiser-scsh--resolve-file-x ()
  (let ((f (geiser-scsh--resolve-file (match-string-no-properties 1))))
    (and (stringp f) (list f))))


;;; REPL startup

(defun geiser-scsh-update-warning-level ()
  "Update the warning level used by the REPL.
The new level is set using the value of `geiser-scsh-warning-level'."
  (interactive)
  (let ((code `(:eval (:ge set-warnings ',geiser-scsh-warning-level)
                      (geiser evaluation))))
    (geiser-eval--send/result code)))

(defun connect-to-scsh ()
  "Start a Scsh REPL connected to a remote process.

Start the external Scsh process with the flag --listen to make
it spawn a server thread."
  (interactive)
  (geiser-connect 'scsh))

(defun geiser-scsh--set-load-path ()
  (let* ((path (expand-file-name "scsh/" geiser-scheme-dir))
         (witness "geiser/emacs.scm")
         ;; (code `(begin (if (not (%search-load-path ,witness))
         ;;                    (set! %load-path (cons ,path %load-path)))
         ;;                'done))
	 (code `(begin (if (not (member ,witness (lib-dirs)))
			   (lib-dirs-append! ,path))
		       'done))
	 )
    (geiser-eval--send/wait code)))

(defun geiser-scsh--startup (remote)
  (set (make-local-variable 'compilation-error-regexp-alist)
       `((,geiser-scsh--path-rx geiser-scsh--resolve-file-x)
         ("^  +\\([0-9]+\\):\\([0-9]+\\)" nil 1 2)))
  (compilation-setup t)
  (font-lock-add-keywords nil
                          `((,geiser-scsh--path-rx 1
						   compilation-error-face)))
  (let* ((geiser-log-verbose-p t)
         (path (expand-file-name "scsh/" geiser-scheme-dir))
	 (load-cmd (format ",config ,load %s" path)))
    (when remote (geiser-scsh--set-load-path))
    ;; (geiser-eval--send/wait ",open geiser-emacs\n")
    (geiser-eval--send/wait load-cmd 5)
    (geiser-eval--send/wait ",open geiser-emacs\n")
    (geiser-scsh-update-warning-level)))


;;; Manual lookup

(defun geiser-scsh--info-spec (&optional nodes)
  (let* ((nrx "^[ 	]+-+ [^:]+:[ 	]*")
         (drx "\\b")
         (res (when (Info-find-file "r5rs" t)
                `(("(r5rs)Index" nil ,nrx ,drx)))))
    (dolist (node (or nodes geiser-scsh-manual-lookup-nodes) res)
      (when (Info-find-file node t)
        (mapc (lambda (idx)
                (add-to-list 'res
                             (list (format "(%s)%s" node idx) nil nrx drx)))
              '("Variable Index" "Procedure Index" "R5RS Index"))))))


(info-lookup-add-help :topic 'symbol :mode 'geiser-scsh-mode
                      :ignore-case nil
                      :regexp "[^()`',\" 	\n]+"
                      :doc-spec (geiser-scsh--info-spec))

(defun scsh--manual-look-up (id mod)
  (let ((info-lookup-other-window-flag
         geiser-scsh-manual-lookup-other-window-p))
    (info-lookup-symbol id 'geiser-scsh-mode))
  (when geiser-scsh-manual-lookup-other-window-p
    (switch-to-buffer-other-window "*info*"))
  (search-forward (format "%s" id) nil t))



;;; Implementation definition:

;; This should load the geiser-specific Scheme code, such as
;; `completions' and friends. Not ready yet though.

;; More study needed.

;; Doesn't seem necessary.

;; This should work fine after the Info manual is ready. Don't forget
;; to search the Scheme 48 manual and R5RS as well!

(define-geiser-implementation scsh
  (arglist geiser-scsh--parameters)
  (repl-startup geiser-scsh--startup)
  (enter-debugger geiser-scsh--enter-debugger)

  (external-help scsh--manual-look-up)
;;
  (binary geiser-scsh--binary)
  (prompt-regexp geiser-scsh--prompt-regexp)
  (debugger-prompt-regexp geiser-scsh--debugger-prompt-regexp)
  (find-module geiser-scsh--get-module)
  (enter-command geiser-scsh--enter-command)
  (exit-command geiser-scsh--exit-command)
  (import-command geiser-scsh--import-command)
  (find-symbol-begin geiser-scsh--symbol-begin)
  (display-error geiser-scsh--display-error)
  (check-buffer geiser-scsh--guess)
  (marshall-procedure geiser-scsh--geiser-procedure)
  (keywords geiser-scsh--keywords)
  (case-sensitive geiser-scsh-case-sensitive-p))

(geiser-impl--add-to-alist 'regexp "\\.scm$" 'scsh t)


(provide 'geiser-scsh)
