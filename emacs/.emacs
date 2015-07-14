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
						  'zenburn-theme
						  'ace-window
						  'theme-changer
						  'evil-nerd-commenter
						  'web-mode
						  'js2-mode
						  )
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives
			 '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(elpy-enable)

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

(require 'theme-changer)
(setq calendar-location-name "Charlottesville, VA")
(setq calendar-latitude 38.04)
(setq calendar-longitude -78.5)
(change-theme 'solarized-light 'zenburn)


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
;; (setq-default tab-width 4 indent-tabs-mode t)
;; (setq-default c-basic-offset 4 c-default-style "linux")
;; (define-key global-map (kbd "RET") 'newline-and-indent)

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

;; Like NerdTree
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
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; ==================
;; Personal shortcuts
;; ==================
(global-set-key (kbd "C-c C-b") 'eval-buffer)
(global-set-key (kbd "C-c C-t") 'transpose-chars)
(global-set-key (kbd "C-c n") 'indent-region)
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "<mouse-4>") 'scroll-down)
(global-set-key (kbd "<mouse-5>") 'scroll-up)
(global-set-key (kbd "C-a") 'beginning-of-line)
(global-set-key (kbd "C-l") 'end-of-line)
(global-set-key (kbd "C-c r") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c t") 'multi-term)
(setq multi-term-program "/bin/bash")
;; magit config
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)

(eval-after-load 'magit
  '(progn
	 (set-face-foreground 'magit-diff-add "green3")
	 (set-face-foreground 'magit-diff-del "red3")
	 (unless window-system
	          (set-face-background 'magit-item-highlight "black"))))
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

(setq c-basic-indent 2)
(setq tab-width 2)
(setq indent-tabs-mode nil)

(projectile-global-mode)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

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
 '(ansi-term-color-vector
   [unspecified "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#d3d0c8"])
 '(custom-safe-themes
   (quote
	("b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" "6ebb2401451dc6d01cd761eef8fe24812a57793c5ccc427b600893fa1d767b1d" "83279c1d867646c5eea8a804a67a23e581b9b3b67f007e7831279ed3a4de9466" "13f85dabe9c9abd73426f190aeedb7d0ad32d29e1fef3138ec8a2435a8fb0910" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "f11b028f78c8934c4dea255d94c491f7ced8720db594f9454dbec55938af3934" "1c57936ffb459ad3de4f2abbc39ef29bfb109eade28405fa72734df1bc252c13" default)))
 '(fci-rule-color "#3a3a3a")
 '(inhibit-startup-screen t)
 '(magit-use-overlays nil)
 '(python-check-command "/usr/local/bin/pyflakes")
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
