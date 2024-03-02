(setq ring-bell-function 'ignore)

(tool-bar-mode -1)

;; Display line numbers
(global-display-line-numbers-mode 1)

;; Auto reload from disk
(global-auto-revert-mode 1)

;; Auto reload Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
