(setq ring-bell-function 'ignore)

(tool-bar-mode -1)

;; Display line numbers
(global-display-line-numbers-mode 1)

;; Auto reload from disk
(global-auto-revert-mode 1)

;; Auto reload Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Font size 15
(set-face-attribute 'default nil :height 172)

;; Maximize window
(toggle-frame-maximized)

;; Save minibuffer prompts history
(setq history-length 25)
(savehist-mode 1)

