;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

(setq native-comp-async-report-warnings-errors nil)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;; UI

(setq save-interprogram-paste-before-kill t
      backup-by-copying t
      ring-bell-function 'ignore
      frame-inhibit-implied-resize t)

(setq-default
 indent-tabs-mode nil
 cursor-type 'bar)

;; Font
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-15"))
(add-to-list 'default-frame-alist '(line-spacing . 0.2))

;; Maximize window
(toggle-frame-maximized)
(load-theme 'tango-dark)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

;; Line numbers in prog mode
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

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

;; Typing with an active selection overwrites
(delete-selection-mode t)

;; Store backups in user-emacs-directory/backups
(unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))

;;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; flycheck

(use-package flycheck
  :hook prog-mode)

;; ivy

(use-package diminish)

(use-package counsel
  :diminish ivy-mode counsel-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-extra-directories nil)
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1)
  (counsel-mode 1))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setq ivy-format-function 'ivy-format-function-line))

;; swift

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode))

;; yaml

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode))

;; helpful

(use-package helpful
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; magit

(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-define-global-key-bindings 'recommended))

;; evil-nerd-commenter

(use-package evil-nerd-commenter
  :bind ("s-/" . evilnc-comment-or-uncomment-lines))

;; company

(use-package company
  :config (global-company-mode))

(use-package expand-region
  :bind (("M-<up>" . 'er/expand-region)
	 ("M-<down>" . 'er/contract-region))
  :defer t)

;; Keybindings

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

(defun my-join-line (&optional arg beg end)
  "Join next line into current one."
  (interactive)
  (if (use-region-p)
      (join-line 'nil (region-beginning) (region-end))
    (join-line 1)))

(defun my-kill-line-or-region ()
  "If nothing selected, kill whole line, else kill region."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(bind-keys
 ("s-Z" . undo-redo)
 ("C-J" . my-join-line)
 ("s-x" . my-kill-line-or-region)
 ("s-A" . counsel-M-x)
 ("s-O" . project-find-file)
 ("M-s-l" . indent-region)
 ("s-<f12>" . imenu)
 ("s-d" . duplicate-line-or-region)
 ("s-f" . swiper-isearch)
 ("s-F" . counsel-ag)
 ("M-z" . zap-up-to-char))

;;;

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 4 1024 1024))

;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(expand-region evil-nerd-commenter company magit helpful ivy-rich flycheck yaml-mode swift-mode counsel diminish ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
