;; ==============
;; Packages
;; ==============
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
	 (if (package-installed-p package)
		 nil
	   (if (y-or-n-p (format "Package %s is missing. Install it? " package))
		   (package-install package)
		 package)))
   packages))

;; Activate installed packages
(package-initialize)

(ensure-package-installed 'evil
						  'magit
						  'key-chord
						  'powerline
						  'neotree
						  'auto-complete
						  'elpy
						  'flymake
						  'solarized-theme
						  'ace-window
						  'evil-nerd-commenter
						  'web-mode
						  )
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives
			 '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(elpy-enable)

;; ==============
;; El-get
;; ==============
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
	(goto-char (point-max))
	(eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(setq package-enable-at-startup nil)

;; ==============
;; Pymacs
;; ==============

;; Run M-x el-get pymacs first
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport 't)
(global-set-key (kbd "M-+") 'rope-auto-import)

;; ==============
;; Flymake
;; ==============
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
					   'flymake-create-temp-inplace))
		   (local-file (file-relative-name
						temp-file
						(file-name-directory buffer-file-name))))
	  (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
			   '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Flymake errors

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
	(let ((help (get-char-property (point) 'help-echo)))
	  (if help (message "%s" help)))))

(defun flymake-html-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
					 'flymake-create-temp-inplace))
		 (local-file (file-relative-name
					  temp-file
					  (file-name-directory buffer-file-name))))
	(list "tidy" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
			 '("\\.html$\\|\\.ctp" flymake-html-init))

(add-to-list 'flymake-err-line-patterns
			 '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
			   nil 1 2 4))


(add-hook 'post-command-hook 'my-flymake-show-help)

;; ==============
;; Themes
;; ==============

(add-to-list 'load-path "~/.emacs.d/themes/material")
(load-theme 'material-light t)

;;===========================
;; Vim emulation and settings
;;==========================

;; vi emulation Yay modal editing
(require 'evil)
(evil-mode 1)
(ido-mode 1)

;; jk to exit insert mode
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'evil-normal-state)

;; vim-like indenting
(setq-default tab-width 4 indent-tabs-mode t)
(setq-default c-basic-offset 4 c-default-style "linux")
(define-key global-map (kbd "RET") 'newline-and-indent)

;; same as map ; :
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)

;;===================
;; Visual settings
;;===================
;; no backup files
(setq make-backup-files nil)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; pretty status bar
(require 'powerline)

;; case insensitive file opening
(set 'read-file-name-completion-ignore-case 1)
(setq ring-bell-function 'ignore)

;; turn on automatic bracket insertion by pairs. New in emacs 24
(electric-pair-mode 1)

;; Enable mouse support
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

										;Like NerdTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; ==================
;; Auto complete
;; ==================
(require 'auto-complete)
(global-auto-complete-mode t)
(add-hook 'python-mode-hook
		  (lambda ()
			(add-to-list 'ac-sources 'ac-source-ropemacs)))

;; ==================
;; Personal shortcuts
;; ==================
(global-set-key (kbd "C-c C-b") 'eval-buffer)
(global-set-key (kbd "C-c C-t") 'transpose-chars)
(global-set-key (kbd "C-c n") 'indent-region)
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "<mouse-4>") 'scroll-down)
(global-set-key (kbd "<mouse-5>") 'scroll-up)
;; magit config
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "<f1> SPC") 'magit-status)

;; custom mode for inserting closing brace and indenting in C and PHP
(define-minor-mode c-helpers-minor-mode
  "This mode contains little helpers for C developement"
  nil
  ""
  '(((kbd "{") . insert-c-block-parentheses)))

(defun insert-c-block-parentheses ()
  (interactive)
  (insert "{")
  (newline)
  (newline)
 (insert "}")
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command))

(add-hook 'c++-mode-hook 'c-helpers-minor-mode)
(add-hook 'php-mode-hook 'c-helpers-minor-mode)



;; ==================
;; Custom-set-variables
;; ==================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#262626"))
 '(custom-safe-themes
   (quote
	("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "f11b028f78c8934c4dea255d94c491f7ced8720db594f9454dbec55938af3934" "1c57936ffb459ad3de4f2abbc39ef29bfb109eade28405fa72734df1bc252c13" default)))
 '(fci-rule-color "#3a3a3a")
 '(inhibit-startup-screen t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#f36c60")
	 (40 . "#ff9800")
	 (60 . "#fff59d")
	 (80 . "#8bc34a")
	 (100 . "#81d4fa")
	 (120 . "#4dd0e1")
	 (140 . "#b39ddb")
	 (160 . "#f36c60")
	 (180 . "#ff9800")
	 (200 . "#fff59d")
	 (220 . "#8bc34a")
	 (240 . "#81d4fa")
	 (260 . "#4dd0e1")
	 (280 . "#b39ddb")
	 (300 . "#f36c60")
	 (320 . "#ff9800")
	 (340 . "#fff59d")
	 (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
