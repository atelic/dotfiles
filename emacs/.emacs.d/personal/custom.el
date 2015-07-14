(prelude-require-packages '(multiple-cursors phi-search geiser json-mode js2-mode rainbow-mode elisp-slime-nav rainbow-delimiters company smex ido-ubiquitous flx-ido vkill exec-path-from-shell zop-to-char zenburn-theme volatile-highlights undo-tree smartrep smartparens operate-on-number move-text magit projectile ov guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl easy-kill diminish diff-hl discover-my-major dash browse-kill-ring anzu ace-window))

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

;;======================
;; Visual settings

;; no backup files
(setq make-backup-files nil)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
;; case insensitive file opening
(set 'read-file-name-completion-ignore-case 1)
(setq ring-bell-function 'ignore)

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

(add-hook 'c++-mode-hook 'c-helpers-minor-mode)
(add-hook 'php-mode-hook 'c-helpers-minor-mode)

(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; Code completion is great
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)


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
;; magit config
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c i") 'magit-status)

(require 'theme-changer)
(setq calendar-location-name "Charlottesville, VA")
(setq calendar-latitude 38.04)
(setq calendar-longitude -78.5)
(change-theme 'solarized-light 'zenburn)

(require 'yasnippet)
(yas-global-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
