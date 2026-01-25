;;; init.el --- SK Emacs Setting -*- lexical-binding: t -*-
;;
;;; Code:

(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (config-el    (expand-file-name "config.el"  user-emacs-directory))
      (config-org   (expand-file-name "config.org" user-emacs-directory))
      (config-local (expand-file-name "config-local.el" user-emacs-directory))
      (pinstall     (expand-file-name "pinstall.el" user-emacs-directory)))

  (when (member "-pinit" command-line-args)
    (setq command-line-args (delete "-pinit" command-line-args))
    (require 'pinstall pinstall)
    (pinstall-init))

  (unless (boundp 'sk-early-init)
    (startup-redirect-eln-cache ".local/eln-cache/"))

  (when (file-newer-than-file-p config-org config-el)
    (package-initialize)
    (require 'org)
    (org-babel-tangle-file config-org))

  (require 'config config-el)

  (load config-local t))

;;; init.el ends here
