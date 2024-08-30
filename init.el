(load-theme 'manoj-dark t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)

(setq debug-on-error t)  ;; Enable verbose error messages
(global-unset-key (kbd "C-x m"))	;; Disable emails
(let ((config-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "eshell-config.el" config-dir))  ;; Load eshell config
  (load-file (expand-file-name "editor-config.el" config-dir))  ;; Load editor config
  (load-file (expand-file-name "dired-config.el" config-dir)))  ;; Load dired config

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-show-dot-for-dired t)
