(load-theme 'manoj-dark t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Enable verbose error messages
(setq debug-on-error t)

;; Load Eshell configuration
(load-file "/home/master/configs/emacs/eshell-config.el")

;; Load Editor configuration
(load-file "/home/master/configs/emacs/editor-config.el")

;; Load Dired configuration
(load-file "/home/master/configs/emacs/dired-config.el")

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-show-dot-for-dired t)
