;;; init.el --- SK Emacs Setting -*- lexical-binding: t -*-
;;
;;; Code:

(unless (boundp 'sk-early-init)
  (load (locate-user-emacs-file "early-init"))) ; Must load early-init.el

(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (config-el    (expand-file-name "config.el"  emacs-config-dir))
      (config-org   (expand-file-name "config.org" emacs-config-dir))
      (config-local (expand-file-name "config-local.el" emacs-config-dir))
      (pinstall     (expand-file-name "pinstall.el" emacs-config-dir)))

  (when (member "-pinit" command-line-args)
    (setq command-line-args (delete "-pinit" command-line-args))
    (require 'pinstall pinstall)
    (pinstall-init))

  (when (file-newer-than-file-p config-org config-el)
    (package-initialize)
    (require 'org)
    (org-babel-tangle-file config-org))

  (require 'config config-el)

  (load config-local t))

;;; init.el ends here
