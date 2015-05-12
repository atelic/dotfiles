;; Include needed files
(add-to-list 'load-path "~/.emacs.d/modes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

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
(setq-default tab-width 4 indent-tabs-mode nil)
(setq-default c-basic-offset 4 c-default-style "bsd")
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

;; Plugin to google selected text 
(google-this-mode 1)
(global-set-key (kbd "C-x g") 'google-this-mode-submap)
