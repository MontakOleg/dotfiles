;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

;; (setq use-package-verbose t)

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
      frame-inhibit-implied-resize t
      inhibit-startup-screen t)

(setq-default
 indent-tabs-mode nil
 cursor-type 'bar)

;; Font
(setq-default line-spacing 0.1)
(set-face-attribute 'default nil
                    ;; :family "JetBrains Mono"
                    :family "Fira Code"
                    :height 150)

;; Maximize window
(toggle-frame-maximized)

;; Themes
(use-package emacs
  :config
  (require-theme 'modus-themes)
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (modus-themes-load-theme 'modus-operandi-tinted))

;; (disable-theme 'modus-operandi-tinted)
;; (load-theme 'tango-dark)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(electric-pair-mode 1)

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

;; Ediff
(setq
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain)

;; Store backups in user-emacs-directory/backups
(unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))

;; Code folding (collapse / expand)

(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))
(global-set-key (kbd "s-=") 'hs-show-block)
(global-set-key (kbd "s-+") 'hs-show-all)
(global-set-key (kbd "s--") 'hs-hide-block)
(global-set-key (kbd "s-_") 'hs-hide-all)

;; Minibuffer

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; flymake

(use-package flymake
  :hook prog-mode
  :bind (("<f2>" . flymake-goto-next-error)
         ("<S-f2>" . flymake-goto-prev-error)))

;; vertico

(use-package vertico
  :config
  (setq vertico-resize nil)
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-x b" . consult-buffer)))

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult)

(recentf-mode 1)

;; swift

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '(swift-mode . my-swift-mode:eglot-server-contact)))
;; yaml

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode))

;; helpful

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; magit

(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk 'all))

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

;; multiple-cursors

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this-word)
         ("C-<" . 'mc/mark-previous-like-this-word)
         ("C-c C-<" . 'mc/mark-all-like-this-dwim)
         :map mc/keymap
         ("<return>" . nil)
         ("<escape>" . mc/keyboard-quit)))

;; dumb-jump

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'ag))

;; wgrep

(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit))
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

;;; Utils

(defun command-output-to-string (command &rest args)
  "Like `shell-command-to-string' but dropping error output.

Also trims whitespace from the ends of any output."
  (string-trim
   (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process command nil '(t nil) nil args)))))

;;; Xcode + Eglot

(defun my-swift-mode:xcrun (&rest args)
  "Invoke xcrun with the given ARGS.

The result is returned as a string."
  (apply #'command-output-to-string "xcrun" args))

;;;###autoload
(defun my-swift-mode:eglot-server-contact (_ignored)
  "Locate the sourcekit-lsp executable in the active Xcode
installation and return its path."
  (list (my-swift-mode:xcrun "--find" "sourcekit-lsp")))

;;; End of Xcode + Eglot

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
 ("s-O" . project-find-file)
 ("M-s-l" . indent-region)
 ("s-<f12>" . imenu)
 ("s-d" . duplicate-line-or-region)
 ("s-f" . consult-line)
 ("s-F" . consult-ripgrep)
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
 '(custom-safe-themes
   '("c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2" default))
 '(package-selected-packages
   '(embark-consult embark orderless consult marginalia vertico modus-themes wgrep dumb-jump multiple-cursors expand-region evil-nerd-commenter company magit helpful yaml-mode swift-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
