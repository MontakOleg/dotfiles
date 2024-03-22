;;; UI

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Maximize window
(toggle-frame-maximized)
(load-theme 'tango-dark)

;; Font size
(set-face-attribute 'default nil :height 154)
;; (set-face-attribute 'default nil :height 172) ; 120*1.2*1.2

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

;; Autosave right in files, not in #files#
(auto-save-visited-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; flycheck

(use-package flycheck
  :init (global-flycheck-mode))

;; ivy

(use-package diminish)

(use-package counsel
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-extra-directories nil)
  (ivy-mode 1)
  (counsel-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line))

;; swift

(use-package swift-mode)

;; yaml

(use-package yaml-mode)

;; helpful

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; magit

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-define-global-key-bindings 'recommended))

;; evil-nerd-commenter

(use-package evil-nerd-commenter
  :bind ("s-/" . evilnc-comment-or-uncomment-lines))

;; company

(use-package company
  :config
  (global-company-mode))

;;;

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd "s-d") 'duplicate-line-or-region)
(global-set-key (kbd "s-f") 'swiper-isearch)

;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-nerd-commenter company magit helpful ivy-rich flycheck yaml-mode swift-mode counsel diminish ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
