;;; early-init.el -*- lexical-binding: t -*-
;;
;;; Code:

;; Redirect `user-emacs-directory'
(defvar emacs-config-dir user-emacs-directory)
(defvar emacs-etc-dir (expand-file-name "etc/" emacs-config-dir))
(setq package-user-dir (expand-file-name "elpa/" emacs-config-dir)
      user-emacs-directory (expand-file-name ".local/" emacs-config-dir))

;; Redirect eln-cache folder
(startup-redirect-eln-cache "eln-cache/")

;; Default settings
(setq default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil))
      package-enable-at-startup nil
      package-install-upgrade-built-in t
      package-native-compile t
      sk-early-init t)
