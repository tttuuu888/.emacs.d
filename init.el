;;; init.el --- SK Emacs Setting -*- lexical-binding: t -*-
;;
;;; Code:

(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (config-el  (expand-file-name "config.el"  user-emacs-directory))
      (config-org (expand-file-name "config.org" user-emacs-directory))
      (local-dir  (expand-file-name ".local/" user-emacs-directory)))

  (unless (file-exists-p local-dir) (make-directory-internal local-dir))
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" local-dir))

  (when (file-newer-than-file-p config-org config-el)
    (package-initialize)
    (require 'org)
    (org-babel-tangle-file config-org))

  (require 'config config-el))

;;; init.el ends here
