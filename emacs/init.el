;;; UI

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Maximize window
(toggle-frame-maximized)
(load-theme 'tango-dark)

;; Font size
(set-face-attribute 'default nil :height 172) ; 120*1.2*1.2

(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)

;; Make ESC quit promts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Auto reload from disk
(global-auto-revert-mode 1)

;; Auto reload Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Save minibuffer prompts history
(setq history-length 25)
(savehist-mode 1)

;;; Packages

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Projectile

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

;; Ivy

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ivy projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
