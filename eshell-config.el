(require 'eshell)
;; Custom prompt
(require 'vc-git)

(defun my-eshell-prompt-function ()
  "Custom Eshell prompt including current directory and Git branch."
  (let* ((current-dir (file-name-nondirectory (directory-file-name (eshell/pwd))))
	 (git-branch (vc-git-working-revision (eshell/pwd)))
	 (branch (if git-branch
		     (format "[%s]" git-branch)
		   "[out-of-repo]"))
	 (prompt (concat
		  (propertize current-dir 'face `(:foreground "#FF5733")) " "
		  (propertize branch 'face `(:foreground "#33FF57")) " "
		  (propertize "> " 'face `(:foreground "#3357FF")))))
    (concat prompt)))

(setq eshell-prompt-function 'my-eshell-prompt-function)
(setq eshell-highlight-prompt nil)

;; Enable history navigation with up/down arrows

;; Set the prompt regexp to match the custom prompt
(setq eshell-prompt-regexp "^[^ ]+ \\[[^]]*\\] > ")

(add-hook 'eshell-mode-hook
  (lambda ()
    (define-key eshell-mode-map (kbd "<up>") 'eshell-previous-input)
    (define-key eshell-mode-map (kbd "<down>") 'eshell-next-input)))

;; Set the location of the Eshell history file
(setq eshell-history-file-name "/home/master/configs/eshell_history")

;; Enable pcomplete for autocompletion
(require 'pcomplete)

;; Bind Alt+p and Alt+n for history search
(add-hook 'eshell-mode-hook
(lambda ()
(define-key eshell-mode-map (kbd "M-p") 'eshell-previous-matching-input-from-input)
(define-key eshell-mode-map (kbd "M-n") 'eshell-next-matching-input-from-input)))
