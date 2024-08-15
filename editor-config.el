(setq-default tab-width 4)

(setq shift-select-mode t)  ;; Enable shift selection
(transient-mark-mode 1)

(setq scroll-step 1)  ;; scroll one line at a time					
(setq scroll-conservatively 10000)  ;; do not put cursor at the center when scrolling	

(defun my-copy-and-keep-selection ()
	"Copy the region and keep the selection active."
	(interactive)
	(if (use-region-p)
		(progn (kill-ring-save (region-beginning) (region-end))
		(setq deactivate-mark nil))
		(message "No region selected")))

;; Fix TAB behavior
(setq-default indent-tabs-mode t)
(global-set-key (kbd "TAB") 'self-insert-command)

;; Bind the custom function to M-w
(global-set-key (kbd "M-w") 'my-copy-and-keep-selection)

;; Enable global line numbers
(global-display-line-numbers-mode t)

;; Enable line numbers only in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun duplicate-line()	;; Duplicate current line
  "Duplicate the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "C-d") 'duplicate-line)

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

(defun my-remove-first-tab-or-spaces (line)
"Remove the first tab character from the line."
(if (string-prefix-p "\t" line)
(substring line 1)
line))
;; (defun my-remove-first-tab-or-spaces (line)
;; "Remove the first tab character or the appropriate number of spaces from the line."
;; (let ((tab-width (or tab-width 4)))  ;; Use the default tab width if not set
;; (cond
;; ;; If the line starts with a tab, remove it
;; ((string-prefix-p "\t" line)
;; (substring line 1))
;; ;; If the line starts with spaces and there are more or equal to tab-width, remove tab-width spaces
;; ((and (string-prefix-p (make-string tab-width ?\s) line)
;; (>= (length line) tab-width))
;; (substring line tab-width))
;; ;; If the line starts with fewer than tab-width spaces, remove all leading spaces
;; ((string-match "^\\s-+" line)
;; (replace-regexp-in-string "^\\s-+" "" line))
;; ;; Otherwise, return the line unchanged
;; (t line))))

(defun my-indent-rigidly-left (start end)
"Indent the region to the left by removing the first tab character from each line."
(interactive "r")
(save-excursion
(let ((deactivate-mark nil))  ;; Keep the region active
(goto-char start)
(while (< (point) end)
(let ((line-start (line-beginning-position))
(line-end (line-end-position)))
(when (and (>= end line-start) (<= start line-end))
(let ((line (buffer-substring line-start line-end)))
(delete-region line-start line-end)
(insert (my-remove-first-tab-or-spaces line))))
(forward-line 1)))
(when (and (not (use-region-p)) (bolp))
(let ((line-start (line-beginning-position))
(line-end (line-end-position)))
(let ((line (buffer-substring line-start line-end)))
(delete-region line-start line-end)
(insert (my-remove-first-tab-or-spaces line))))))))

(global-set-key (kbd "<backtab>") 'my-indent-rigidly-left)

(defun my-add-tab-or-spaces (line)
"Add a tab character at the beginning of the line."
(concat "\t" line))

(defun my-indent-rigidly-right (start end)
"Indent the region to the right by adding a tab character to each line."
(interactive "r")
(save-excursion
(let ((deactivate-mark nil))  ;; Keep the region active
(if (use-region-p)
(progn
(goto-char start)
(while (< (point) end)
(let ((line-start (line-beginning-position))
(line-end (line-end-position)))
(when (and (>= end line-start) (<= start line-end))
(let ((line (buffer-substring line-start line-end)))
(delete-region line-start line-end)
(insert (my-add-tab-or-spaces line))))
(forward-line 1))))
;; Handle the case where no region is selected
(progn
(insert "\t")
(forward-char 1))))))

(global-set-key (kbd "TAB") 'my-indent-rigidly-right)  ;; Bind Tab to the function

;; Move line/selection up/down:

(defun move-line-up ()
"Move the current line or selected lines up."
(interactive)
(let ((col (current-column)))
(if (use-region-p)
(let ((start (region-beginning))
(end (region-end)))
(save-excursion
(goto-char start)
(beginning-of-line)
(setq start (point))
(goto-char end)
(unless (bolp) (end-of-line))
(setq end (point))
(let ((region (buffer-substring start end)))
(delete-region start end)
(forward-line -1)
(beginning-of-line)
(insert region)
(newline)
(set-mark (point))
(goto-char start)
(forward-line -1)
(end-of-line)
(exchange-point-and-mark))))
(progn
(beginning-of-line)
(let ((line (buffer-substring (line-beginning-position) (line-end-position))))
(delete-region (line-beginning-position) (line-end-position))
(delete-char 1)  ;; Remove the newline character
(forward-line -1)
(end-of-line)
(newline)
(insert line)
(move-to-column col))))))

(defun move-line-down ()
"Move the current line or selected lines down."
(interactive)
(let ((col (current-column)))
(if (use-region-p)
(let ((start (region-beginning))
(end (region-end)))
(save-excursion
(goto-char start)
(beginning-of-line)
(setq start (point))
(goto-char end)
(unless (bolp) (end-of-line))
(setq end (point))
(let ((region (buffer-substring start end)))
(delete-region start end)
(forward-line 1)
(beginning-of-line)
(insert region)
(newline)
(set-mark (point))
(goto-char start)
(forward-line 1)
(end-of-line)
(exchange-point-and-mark))))
(progn
(beginning-of-line)
(let ((line (buffer-substring (line-beginning-position) (line-end-position))))
(delete-region (line-beginning-position) (line-end-position))
(delete-char 1)  ;; Remove the newline character
(forward-line 1)
(beginning-of-line)
(insert line)
(newline)
(forward-line -1)
(move-to-column col))))))



(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

