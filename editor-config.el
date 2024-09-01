(setq-default tab-width 4)

(setq shift-select-mode t)  ;; Enable shift selection
(transient-mark-mode 1)
(delete-selection-mode 1)

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


;; Enable global line numbers
(global-display-line-numbers-mode t)

;; Enable line numbers only in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun duplicate-line()
	"Duplicate the selected region or the current line if no region is selected."
	(interactive)
	(if (use-region-p)
		(let* ((reg-start (mark))
			   (reg-end (point))
			   (start (save-excursion (goto-char (region-beginning)) (line-beginning-position)))
			   (end (save-excursion (goto-char (region-end)) (line-end-position)))
			   (reg-length (1+ (- end start))))
			(copy-region-as-kill start end)
			(goto-char start)
			(yank)
			(newline)
			(goto-char (+ reg-start reg-length))
			(set-mark (point))
			(goto-char (+ reg-end reg-length))
			(setq deactivate-mark nil))
		(let ((line-start (line-beginning-position))
			  (line-end (line-end-position)))
			(copy-region-as-kill line-start line-end)
			(goto-char line-start)
			(yank)
			(newline))))


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


;; Forward tab
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


;; Move line/selection up/down:
(defun move-line-up ()
	"Move the current line or selected lines up."
	(interactive)
	(let ((start (if (use-region-p) (copy-marker (region-beginning)) (point-marker)))
		  (end (if (use-region-p) (copy-marker (region-end)) (point-marker))))
		(if (use-region-p)
			(progn
				(goto-char start)
				(forward-line -1)
				(let ((temp (delete-and-extract-region (line-beginning-position) (line-end-position))))
					(delete-char 1)
				  	(goto-char end)
					(goto-char (line-end-position))
					(insert "\n" temp)))
			(progn
				(forward-line -1)
				(let ((temp (delete-and-extract-region (line-beginning-position) (line-end-position))))
					(delete-char 1)
				  	(goto-char (line-end-position))
					(insert "\n" temp))))
		(goto-char start)
		(set-mark (marker-position end))
		(when (use-region-p)
			(set-mark (marker-position start))
			(goto-char (marker-position end))
			;; (move-to-column col)
			(setq deactivate-mark nil))
		(set-marker start nil)
		(set-marker end nil)))

(defun move-line-down ()
	"Move the current line or selected lines down."
	(interactive)
	(let ((start (if (use-region-p) (copy-marker (region-beginning)) (point-marker)))
		  (end (if (use-region-p) (copy-marker (region-end)) (point-marker))))
		(if (use-region-p)
			(progn
				(goto-char end)
				(forward-line 1)
				(let ((temp (delete-and-extract-region (line-beginning-position) (line-end-position))))
					(delete-char 1)
				  	(goto-char start)
					(goto-char (line-beginning-position))
					(insert temp "\n")))
			(progn
				(forward-line 1)
				(let ((temp (delete-and-extract-region (line-beginning-position) (line-end-position))))
				  	(delete-char 1)
				  	(forward-line -1)
				  	(goto-char (line-beginning-position))
					(insert temp "\n"))))
		(goto-char start)
		(set-mark (marker-position end))
		(when (use-region-p)
			(set-mark (marker-position start))
			(goto-char (marker-position end))
			(setq deactivate-mark nil))
		(set-marker start nil)
		(set-marker end nil)))


;; Search selected text
(defun isearch-forward-selected ()
	"Search for the selected text using isearch."
	(interactive)
	(if (use-region-p)
		(let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
			(deactivate-mark)
			(isearch-mode t)
			(isearch-yank-string selection))
		(call-interactively 'isearch-forward)))


;; Custom highlighting:
(defface my-highlight-yellow
	'((t (:background "#FFFF00" :foreground "#000000")))
	"Face for highlighting with yellow background.")

(defface my-highlight-green
	'((t (:background "#00FF00" :foreground "#000000")))
	"Face for highlighting with green background.")

(defface my-highlight-cyan
	'((t (:background "#00FFFF" :foreground "#000000")))
	"Face for highlighting with cyan background.")

(defface my-highlight-magenta
	'((t (:background "#FF00FF" :foreground "#000000")))
	"Face for highlighting with magenta background.")

(defface my-highlight-red
	'((t (:background "#FF0000" :foreground "#000000")))
	"Face for highlighting with red background.")

(defvar my-highlight-colors
	'(my-highlight-yellow my-highlight-green my-highlight-cyan my-highlight-magenta my-highlight-red)
	"List of faces to use for highlighting.")

;; Highlight on hotkey 
(defun my-highlight-text (color-index)
	"Highlight all occurrences of the selected text in the buffer with a specified color.
	COLOR-INDEX is the index of the color in `my-highlight-colors`."
	(interactive "nEnter color index (1-5, 0 to unhighlight): ")
	(if (and (use-region-p) (> color-index 0))
		(let ((selection (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
			(face (nth (1- color-index) my-highlight-colors)))
			(highlight-regexp selection face))
		(unhighlight-regexp t)))

;; Highlight under cursor
(defvar my-highlight-regexp nil "The current regexp to highlight.")
(custom-set-faces '(hi-yellow ((t (:background "#555500" :foreground "#000000")))))

(defun my-highlight-all-occurrences ()
	"Highlight all occurrences of the word at point."
	(when my-highlight-regexp
		(unhighlight-regexp my-highlight-regexp))
	(let ((word (thing-at-point 'word)))
		(when word
			(setq my-highlight-regexp (regexp-quote word))
			(highlight-regexp my-highlight-regexp 'hi-yellow))))

(add-hook 'post-command-hook 'my-highlight-all-occurrences)


;; My custom minor mode
(define-minor-mode my-custom-keys-mode
	"A minor mode for my custom key bindings."
	:init-value t
	:lighter " MyKeys"
	:keymap (let ((map (make-sparse-keymap)))
	;; (define-key map (kbd "C-d") 'your-custom-function)
	;; Set key bindings here
	;; (global-set-key (kbd "TAB") 'self-insert-command)
	;; (global-set-key (kbd "M-w") 'my-copy-and-keep-selection)
	;; (global-set-key (kbd "C-d") 'duplicate-line)
	;; (global-set-key (kbd "C-/") 'my-comment-or-uncomment-region-or-line)
	;; (global-set-key (kbd "<backtab>") 'my-indent-rigidly-left)
	;; (global-set-key (kbd "TAB") 'my-indent-rigidly-right)  ;; Bind Tab to the function
	;; (global-set-key (kbd "M-<up>") 'move-line-up)
	;; (global-set-key (kbd "M-<down>") 'move-line-down)
	;; (global-set-key (kbd "C-f") 'isearch-forward-selected)
	;; (global-set-key (kbd "C-c h") 'my-highlight-text)
	(define-key map (kbd "TAB") 'self-insert-command)
	(define-key map (kbd "M-w") 'my-copy-and-keep-selection)
	(define-key map (kbd "C-d") 'duplicate-line)
	(define-key map (kbd "C-/") 'my-comment-or-uncomment-region-or-line)
	(define-key map (kbd "<backtab>") 'my-indent-rigidly-left)
	(define-key map (kbd "TAB") 'my-indent-rigidly-right)  ;; Bind Tab to the function
	(define-key map (kbd "M-<up>") 'move-line-up)
	(define-key map (kbd "M-<down>") 'move-line-down)
	(define-key map (kbd "C-f") 'isearch-forward-selected)
	(define-key map (kbd "C-c h") 'my-highlight-text)
map))

(my-custom-keys-mode 1)  ;; Enable the mode globally
