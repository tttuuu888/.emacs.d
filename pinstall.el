;;; pinstall.el --- Parallel package install script -*- lexical-binding: t -*-

;; Copyright (C) 2026 Seungki Kim

;; Author: Seungki Kim <tttuuu888@gmail.com>
;; URL: https://github.com/tttuuu888/.emacs.d
;; Version: 0.7.0

;;; Commentary

;; This script installs all external packages managed by `use-package' as
;; parallel processes.

;; Usage:
;; To start `pinstall' from shell, use below command:
;;
;;  $ emacs -l ~/.emacs.d/pinstall.el -batch -init
;;
;; To start `pinstall' directly in emacs, use as below:
;;
;;  (require 'pinstall)
;;  (pinstall-init)

;;; Code:

(require 'use-package)

(defvar pinstall-process-number (max 8 (1- (num-processors))))

(defvar pinstall-package-list nil)

(defvar pinstall-vc-package-list nil)

(defconst pinstall-file load-file-name)

(defun package-archives-init ()
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(defun get-package-list ()
  (defmacro use-package (pkg &rest args)
    (let ((ensure (memq :ensure args))
          (vc-arg (plist-get args :vc))
          (disabled (memq :disabled args))
          (always-ensure (bound-and-true-p use-package-always-ensure)))
      (unless (or disabled
                  (if always-ensure
                      (and ensure (eq nil (cadr ensure)))
                    (or (not ensure) (eq nil (cadr ensure)))))
        (if vc-arg
            `(add-to-list 'pinstall-vc-package-list (cons ',pkg ',vc-arg))
          `(add-to-list 'pinstall-package-list ',pkg)))))
  (cl-letf (((symbol-function 'package-install)
             (lambda (&rest _) nil))
            ((symbol-function 'package-refresh-contents)
             (lambda (&rest _)) nil))
    (load (expand-file-name "init.el" user-emacs-directory)))
  (package-archives-init)
  pinstall-package-list)

(defun remove-duplicate-packages-in-depth (packages &optional depth)
  (let* ((result packages)
         (base-depth (if depth depth 0))
         (base-packages (nth base-depth packages)))
    (if (< base-depth (length packages))
        (progn
          (dotimes (dep (length packages))
            (when (> dep base-depth)
              (let ((target-packages (nth dep result)))
                (mapc (lambda (x)
                        (setq target-packages (delq x target-packages)))
                        base-packages)
                (setcar (nthcdr dep result) target-packages))))
          (remove-duplicate-packages-in-depth result (1+ base-depth)))
      packages)))

(defun get-dependency-package-list ()
  (let* ((all-package-list (get-package-list))
         (dep-packages all-package-list)
         (no-dep-packages (list))
         (result (list (seq-filter (lambda (x) (not (package-installed-p x)))
                                   all-package-list))))
    (while dep-packages
      (let ((deps
             (cl-delete-duplicates
              (mapcan
               (lambda (pkg)
                 (let* ((desc (car (cdr (assq pkg package-archive-contents))))
                        (reqs (when desc
                                (seq-filter
                                 (lambda (x) (not (package-installed-p x)))
                                 (mapcar #'car (package-desc-reqs desc))))))
                   (when (and (not reqs) (not (package-installed-p pkg)))
                     (cl-pushnew pkg no-dep-packages))
                   reqs))
               dep-packages))))
        (setq dep-packages deps)
        (when deps (cl-pushnew deps result))))
    (when (or result no-dep-packages)
      (setcar (nthcdr 0 result) (cl-delete-duplicates
                                 (append no-dep-packages (car result)))))
    result))

(defun print-package-installed (package)
  (let* ((pkg-len (length package))
         (padding (make-string (max 1 (- 25 pkg-len)) ?.)))
    (message (format "%s %s..done." package padding))))

(defun packages-installed-p (remained-packages)
  (dolist (pkg remained-packages)
    (when (package-installed-p (intern pkg))
      (print-package-installed pkg)
      (setq remained-packages (remove pkg remained-packages))))
  remained-packages)

(defun init-process-check (package-list proc-list)
  (let ((remained-packages (mapcar #'symbol-name package-list)))
    (while (seq-filter (lambda (proc) (process-live-p proc)) proc-list)
      (package-initialize t)
      (setq remained-packages (packages-installed-p remained-packages))
      (sleep-for 0.5))
    (package-initialize t)
    (packages-installed-p remained-packages)))

(defun async-install-packages (package-list)
  (let* ((proc-list nil)
         (proc-pkgs-list (make-list pinstall-process-number nil))
         (idx 0))
    (dolist (pkg (reverse package-list))
      (cl-pushnew (symbol-name pkg) (nth idx proc-pkgs-list))
      (setq idx (if (= idx (1- pinstall-process-number)) 0 (1+ idx))))
    (dolist (pkgs proc-pkgs-list)
      (when pkgs
        (let ((proc (apply
                     #'start-process
                     "Install"
                     nil
                     "emacs" "-batch" "-Q"
                     "-l" pinstall-file
                     "-install" pkgs)))
          (cl-pushnew proc proc-list))))
    (init-process-check package-list proc-list)))

(defun async-install-vc-packages (vc-package-list)
  (let* ((proc-list nil)
         (proc-pkgs-list (make-list pinstall-process-number nil))
         (idx 0))
    (dolist (spec vc-package-list)
      (cl-pushnew (format "%S" spec) (nth idx proc-pkgs-list))
      (setq idx (mod (1+ idx) pinstall-process-number)))

    (dolist (specs proc-pkgs-list)
      (when specs
        (let ((proc (apply #'start-process "VC-Install" nil "emacs" "-batch"
                           "-Q" "-l" pinstall-file "-vc-install" specs)))
          (push proc proc-list))))
    (init-process-check (mapcar #'car vc-package-list) proc-list)))

(defun init-function (&rest _)
  (delete "-init" command-line-args)
  (package-archives-init)
  (package-refresh-contents)
  (let* ((deps-list (get-dependency-package-list))
         (packages-list (remove-duplicate-packages-in-depth deps-list))
         (all-packages-count (length (apply #'append packages-list))))
    ;; (message (format "Packages to install : %s" packages-list))
    (if (zerop all-packages-count)
        (message "\nAll packages are already installed.")
      (message (format "\nInstall %s packages..." all-packages-count))
      (dolist (packages packages-list)
        (async-install-packages packages))))
  (let ((vc-pkgs
         (seq-filter (lambda (spec) (not (package-installed-p (car spec))))
                     pinstall-vc-package-list)))
    ;; (message (format "VC packages to install : %s" vc-pkgs))
    (if (null vc-pkgs)
        (when pinstall-vc-package-list
          (message "\nAll VC packages are already installed."))
      (message "\nInstall %s VC packages..." (length vc-pkgs))
      (async-install-vc-packages vc-pkgs)))
  (message "Init done."))

(defun install-function (&rest _)
  (delete "-install" command-line-args)
  (package-archives-init)
  (dolist (pkg command-line-args-left)
    (unless (package-installed-p (intern pkg))
      (condition-case err
          (package-install (intern pkg) nil) ;; Do not refresh
        (error (message "Failed to install %s: %s"
                        pkg (error-message-string err)))))))

(defun install-vc-function (&rest _)
  (delete "-vc-install" command-line-args)
  (package-archives-init)
  (dolist (spec-raw command-line-args-left)
    (let ((spec (read spec-raw)))
      (unless (package-installed-p (car spec))
        (condition-case err
            (package-vc-install spec)
          (error (message "Failed to install VC %s: %s"
                          (car spec) (error-message-string err))))))))

(add-to-list 'command-switch-alist '("-init" . init-function))
(add-to-list 'command-switch-alist '("-install" . install-function))
(add-to-list 'command-switch-alist '("-vc-install" . install-vc-function))

;;;###autoload
(defun pinstall-init ()
  (delete "-init" command-line-args)
  (let ((output-buffer (generate-new-buffer "*Init*")))
    (switch-to-buffer output-buffer)
    (call-process "emacs" nil  output-buffer t
                  "-l" pinstall-file "-batch" "-init")
    (package-initialize)))

(provide 'pinstall)
