;;; early-init.el -*- lexical-binding: t -*-
;;
;;; Code:

(setq default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil))
      package-enable-at-startup nil
      package-native-compile t
      sk-early-init t)

(startup-redirect-eln-cache ".local/eln-cache/")
