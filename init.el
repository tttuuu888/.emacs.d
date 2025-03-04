;;; init.el --- SK Emacs Setting -*- lexical-binding: t -*-
;;
;;; Code:

(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (config-el  (expand-file-name "config.el"  user-emacs-directory))
      (config-org (expand-file-name "config.org" user-emacs-directory)))

  (unless (boundp 'sk-early-init)
    (startup-redirect-eln-cache ".local/eln-cache/"))

  (when (file-newer-than-file-p config-org config-el)
    (package-initialize)
    (require 'org)
    (org-babel-tangle-file config-org))

  (require 'config config-el))

;;; init.el ends here
