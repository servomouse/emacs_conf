;; Enable shift selection
(setq shift-select-mode t)
(transient-mark-mode 1)

(defun my-copy-and-keep-selection ()
"Copy the region and keep the selection active."
(interactive)
(if (use-region-p)
(progn
(kill-ring-save (region-beginning) (region-end))
(setq deactivate-mark nil))
(message "No region selected")))

;; Bind the custom function to M-w
(global-set-key (kbd "M-w") 'my-copy-and-keep-selection)

;; Enable global line numbers
(global-display-line-numbers-mode t)

;; Enable line numbers only in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Duplicate current line
(defun duplicate-line()
  "Duplicate the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "C-d") 'duplicate-line)

;; Comment-uncomment line(s)
;; (global-set-key (kbd "C-/") 'comment-line)
;; (defun my-comment-line ()
;; "Comment or uncomment the current line, keeping the cursor in place."
;; (interactive)
;; (save-excursion
;; (comment-line 1)))
;; (global-set-key (kbd "C-/") 'my-comment-line)
(defun my-comment-or-uncomment-region-or-line ()
"Comment or uncomment the current line or region, keeping the cursor in place."
(interactive)
(let ((start (line-beginning-position))
(end (line-end-position)))
(if (use-region-p)
(setq start (save-excursion (goto-char (region-beginning)) (line-beginning-position))
end (save-excursion (goto-char (region-end)) (line-end-position))))
(save-excursion
(comment-or-uncomment-region start end))))
(global-set-key (kbd "C-/") 'my-comment-or-uncomment-region-or-line)


;; BackTab
;; Custom function to remove tab from selected lines
(defun my-indent-rigidly-left (start end)
"Indent the region to the left by 4 spaces, or the current line if no region is selected."
(interactive "r")
(if (use-region-p)
(indent-rigidly start end -4)
(indent-rigidly (line-beginning-position) (line-end-position) -4)))

;; Bind Shift-TAB to the custom function
;; Custom function to remove tab from selected lines
(defun my-indent-rigidly-left (start end)
"Indent the region to the left by 4 spaces, or the current line if no region is selected."
(interactive "r")
(if (use-region-p)
(indent-rigidly start end -4)
(indent-rigidly (line-beginning-position) (line-end-position) -4)))

;; Bind Shift-TAB to the custom function
(global-set-key (kbd "<backtab>") 'my-indent-rigidly-left)
