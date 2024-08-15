(eval-after-load 'dired  ;; Bind `j` to jump to parent directory
  '(define-key dired-mode-map (kbd "j") 'dired-up-directory))

(eval-after-load 'dired  ;; Bind `C-s` for incremental search
  '(define-key dired-mode-map (kbd "C-s") 'isearch-forward))

(setq dired-kill-when-opening-new-dired-buffer t)  ;; Do not create multiple buffers
