;;Highliting for php
(add-to-list 'load-path "~/.emacs.d/modes/")
(require 'php-mode)
;;Zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;;get rid of buttons in GUI mode
(if window-system
  (tool-bar-mode -1)
)
(menu-bar-mode -1)
;; packages
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

(require 'markdown-mode)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)


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

;; Org mode and keybindings
(add-to-list 'load-path "~/.emacs.d/modes/org/lisp")
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
