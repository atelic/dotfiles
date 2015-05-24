;; Include needed files
(add-to-list 'load-path "~/.emacs.d/modes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")


;;Highliting for php
(require 'php-mode)

;;Zenburn theme
(load-theme 'zenburn t)

;;get rid of buttons and menu
(if window-system
  (tool-bar-mode -1)
)
(menu-bar-mode -1)

;; package repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
;;===========================
;; Vim emulation and settings
;;==========================

;;Vi emulation Yay modal editing
(require 'evil)
(evil-mode 1)

;;jk to exit insert mode
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

;;======================
;; Visual settings
;;======================
;; no backup files
(setq make-backup-files nil)
(scroll-bar-mode 1)
;; pretty status bar
(require 'powerline)
(powerline-center-evil-theme)

;; case insensitive file opening
(set 'read-file-name-completion-ignore-case 1)
(setq ring-bell-function 'ignore)

;; Awesome plugin to keep track of lisp parens
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;; Function for inserting closing brace and indenting in C and PHP
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

(add-hook 'php-mode-hook 'c-helpers-minor-mode)
(add-hook 'c++-mode-hook 'c-helpers-minor-mode)

;; turn on automatic bracket insertion by pairs. New in emacs 24
(electric-pair-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-tags-on-save t)
 '(haskell-process-type 'cabal-repl)
 )

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(global-git-gutter-mode +1)

;; Interactivly open files, buffers etc
(require 'ido)
(ido-mode t)

;; I like mouse scrolling in URxvt
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; Multi-line commenting without selecting text
;; Just ,,9j to comment the next 9 lines
(evilnc-default-hotkeys)

;; Try to make gnus not make new ~/ directories
(setq message-directory "~/.emacs.d/mail/")
(setq gnus-directory "~/.emacs.d/news/")
(setq nnfolder-directory "~/.emacs.d/mail/archive")

;; Code completion is great
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(google-this-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(eval-after-load 'haskell-mode
            '(define-key haskell-mode-map [f6] 'haskell-navigate-imports))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(evil-define-key 'insert haskell-interactive-mode-map (kbd "RET") 'haskell-interactive-mode-return)
(evil-define-key 'normal haskell-interactive-mode-map (kbd "RET") 'haskell-interactive-mode-return)

(eval-after-load 'haskell-mode
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load 'haskell-cabal
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(require 'rainbow-delimiters)
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
(server-start t)

(require 'auto-complete-clang-async)
(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)

(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))

(defun notes ()
    "Switch to my work dir."
	 (interactive)
	 (find-file "/home/eric/doc/org"))

(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)

(add-hook 'c-mode-common-hook   'auto-make-header)
(add-hook 'c++-mode-hook   'auto-make-header)



(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

(global-set-key [C-M-down] 'win-resize-minimize-vert)
(global-set-key [C-M-up] 'win-resize-enlarge-vert)
(global-set-key [C-M-left] 'win-resize-minimize-horiz)
(global-set-key [C-M-right] 'win-resize-enlarge-horiz)
(global-set-key [C-M-up] 'win-resize-enlarge-horiz)
(global-set-key [C-M-down] 'win-resize-minimize-horiz)
(global-set-key [C-M-left] 'win-resize-enlarge-vert)
(global-set-key [C-M-right] 'win-resize-minimize-vert)

(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(require 'evil-org)

(global-set-key (kbd "C-c f") 'fiplr-find-file)

(setq org-cycle-include-plain-lists 'integrate)

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(setq ruby-deep-indent-paren nil)
(global-set-key (kbd "C-c r r") 'inf-ruby)

(add-hook 'ruby-mode-hook 'projectile-on)
;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  adefun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
	    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
	a(define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)

(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(require 'doremi)
