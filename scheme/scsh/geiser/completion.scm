;;; completion.scm -- completing known symbols and module names

;; Copyright (C) 2009, 2012 Jose Antonio Ortega Ruiz
;; Copyright (C) 2013 Rich Loveland

;; This file repurposes some Public Domain code from Taylor Campbell's
;; SLIME48, which you can read more about at
;; <http://mumble.net/~campbell/slime48.html>.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(define (ge:completions prefix)
  (let ((completions (car (simple-completions prefix 'NIL))))
    (sort-list completions string<?)))

(define (ge:module-completions prefix)
  (compute-module-completions prefix (gather-all-packages) string-prefix-ci?))

(define (read-from-string string)
      (read (make-string-input-port string)))

(define (gather-all-packages)
  ;; -> List
  (let ((all-packages '()))
    (table-walk
     (lambda (k v) (set! all-packages (cons v all-packages)))
     package-name-table)
    all-packages))

(define (compute-module-completions prefix-string packages completion?)
  (let ((completions '())
        (completion? (make-completion-predicate prefix-string
                                                completion?)))
    (define (test symbol)
      (if (symbol? symbol)     ; protect against generated names
          (let ((string (symbol->string symbol)))
            (if (completion? string)
                (set! completions (cons string completions))))))
    (for-each (lambda (element)
		(test element))
	      packages)
    completions))

(define (simple-completions prefix-string package-spec)
  (let ((package-id (if (string? package-spec)
                        (read-from-string package-spec)
                        package-spec)))
    (cond ((and (eq? package-spec 'NIL)
                    (interaction-environment))
           => (lambda (package)
                (let ((completions
                       (compute-completions prefix-string package
                                            string-prefix-ci?)))
                  (list completions
                        (longest-common-prefix completions)))))
          (else
	   (format #t "No such package by name: ~A" package-spec)))))

(define (compute-completions prefix-string package completion?)
  (let ((completions '())
        (completion? (make-completion-predicate prefix-string
                                                completion?)))
    (define (test symbol)
      (if (symbol? symbol)     ; protect against generated names
          (let ((string (symbol->string symbol)))
            (if (completion? string)
                (set! completions (cons string completions))))))
    (for-each-definition (lambda (symbol binding)
                           (test symbol))
                         package)
    (for-each (lambda (open)
                (for-each-export (lambda (symbol type binding)
                                   (test symbol))
                                 open))
              (package-opens package))
    completions))

(define (make-completion-predicate prefix-string completion?)
  ;++ This is a kind of cheesy hack.  What we really want is for the
  ;++ reader to expose its symbol recognizer.  Fortunately, symbols are
  ;++ GC'd, so this is OK for now.
  (let ((prefix-symbol (string->symbol prefix-string)))
    (cond ((symbol? prefix-symbol)
           (let ((prefix-string (symbol->string prefix-symbol)))
             (lambda (string)
               (completion? prefix-string string))))
          ((eof-object? prefix-symbol)
           (lambda (string)
             string                     ;ignore
             #t))
          (else
           (error "Invalid prefix string: ~S" prefix-string)))))

(define (longest-common-prefix strings)
  (if (null? strings)
      ""
      (fold (lambda (s1 s2)
              (receive (len shorter)
                       (let ((s1-len (string-length s1))
                             (s2-len (string-length s2)))
                         (if (< s1-len s2-len)
                             (values s1-len s1)
                             (values s2-len s2)))
                (let loop ((i 0))
                  (cond ((= i len)
                         shorter)
                        ((char=? (string-ref s1 i)
                                 (string-ref s2 i))
                         (loop (+ i 1)))
                        (else
                         (substring shorter 0 i))))))
            (cdr strings)
            (car strings))))
