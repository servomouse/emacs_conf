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

(defun my-region-active-p ()
	"Return t if the region is active and not empty."
	(and (region-active-p) (> (region-end) (region-beginning))))

;; BackTab
(defun my-indent-rigidly-left (&optional start end)
"Indent the region to the left by removing the first tab character or spaces from each line."
(interactive "r")
(let* ((start (if (my-region-active-p) (region-beginning) (line-beginning-position)))
	   (end (if (my-region-active-p) (region-end) (line-end-position)))
	   (start-marker (copy-marker start t))
(end-marker (copy-marker end t)))
(save-excursion
(goto-char start)
(while (< (point) (marker-position end-marker))
  (let ((line-start (line-beginning-position))
		(line-end (line-end-position))
		(counter tab-width))
  	   (if (string-prefix-p "\t" (buffer-substring-no-properties line-start line-end))
	  		(progn
				(goto-char (line-beginning-position))
	    		(delete-char 1))
  		 (progn
		   		(while (and (> counter 0) (string-prefix-p " " (buffer-substring-no-properties line-start line-end)))
			 		(progn
			   			(goto-char (line-beginning-position))
	    				(delete-char 1)
						(setq counter (1- counter)))))))
  (forward-line 1)
  ;; Update the end marker position
(set-marker end-marker (marker-position end-marker))))
(goto-char start-marker)
(set-mark (marker-position end-marker))
(setq deactivate-mark nil)
;; Clean up markers
(set-marker start-marker nil)
(set-marker end-marker nil)))

(global-set-key (kbd "<backtab>") 'my-indent-rigidly-left)

(defun my-indent-rigidly-right (start end)
"Indent the region to the right by adding a tab character to each line."
(interactive "r")
(if (my-region-active-p)
(let ((start-marker (copy-marker start t))
	  (end-marker (copy-marker end t)))
(save-excursion
(goto-char start)
(while (< (point) (marker-position end-marker))
	(goto-char (line-beginning-position))
	(unless (looking-at-p "^[[:space:]]*$")
	   	(insert "\t"))
	(forward-line 1)
	;; Update the end marker position
	(set-marker end-marker (marker-position end-marker))))
(goto-char start-marker)
(set-mark (marker-position end-marker))
(setq deactivate-mark nil)
;; Clean up markers
(set-marker start-marker nil)
(set-marker end-marker nil))
(insert "\t")))

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

