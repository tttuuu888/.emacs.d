;; SK Emacs Setting

(package-initialize)
(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))


;; use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path "~/.emacs.d/conf.d/")

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'use-package)
(use-package conf-init)

(recentf-open-files)
