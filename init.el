;;; init.el --- SK Emacs Setting -*- lexical-binding: t -*-
;;
;;; Code:

(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (eln-dir    (expand-file-name ".local/eln-cache/" user-emacs-directory))
      (config-el  (expand-file-name "config.el"  user-emacs-directory))
      (config-org (expand-file-name "config.org" user-emacs-directory)))

  (defun ensure-dir (dir)
    "Ensure that DIR exists, creating it if necessary. Returns DIR."
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir)

  (startup-redirect-eln-cache (ensure-dir eln-dir))

  (when (file-newer-than-file-p config-org config-el)
    (package-initialize)
    (require 'org)
    (org-babel-tangle-file config-org))

  (require 'config config-el))

;;; init.el ends here
