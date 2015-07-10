;;; robe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "robe" "robe.el" (21846 46487 641653 593000))
;;; Generated autoloads from robe.el

(autoload 'robe-mode "robe" "\
Improved navigation for Ruby.

The following commands are available:

\\{robe-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "robe-ac" "robe-ac.el" (21846 46487 694988
;;;;;;  17000))
;;; Generated autoloads from robe-ac.el

(autoload 'robe-ac-available "robe-ac" "\
Return t if `robe-mode' completions are available, otherwise nil.

\(fn)" nil nil)

(defconst ac-source-robe '((available . robe-ac-available) (candidates . robe-ac-candidates) (document . robe-ac-doc) (symbol . "r")) "\
`auto-complete' completion source for Ruby using `robe-mode'.")

;;;***

;;;### (autoloads nil "robe-company" "robe-company.el" (21846 46487
;;;;;;  491650 524000))
;;; Generated autoloads from robe-company.el

(autoload 'company-robe "robe-company" "\
A `company-mode' completion back-end for `robe-mode'.

\(fn COMMAND &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("robe-pkg.el") (21846 46487 778025 367000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; robe-autoloads.el ends here
