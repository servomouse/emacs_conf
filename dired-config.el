;; Bind `j` to jump to parent directory
(eval-after-load 'dired
'(define-key dired-mode-map (kbd "j") 'dired-up-directory))

;; Bind `C-s` for incremental search
(eval-after-load 'dired
'(define-key dired-mode-map (kbd "C-s") 'isearch-forward))

;; Do not create multiple buffers
(setq dired-kill-when-opening-new-dired-buffer t)
