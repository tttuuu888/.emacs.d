;;; pinstall.el --- Parallel package install script -*- lexical-binding: t -*-

;; Copyright (C) 2019 SeungKi Kim

;; Author: SeungKi Kim <tttuuu888@gmail.com>
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

(defvar pinstall-process-number 8)

(defvar pinstall-package-list nil)

(defconst pinstall-file load-file-name)

(defun package-archives-init ()
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize t))

(defun get-package-list ()
  (add-to-list 'pinstall-package-list 'use-package)
  (defmacro use-package (pkg &rest args)
    (let ((ensure (memq :ensure args))
          (disabled (memq :disabled args))
          (always-ensure (bound-and-true-p use-package-always-ensure)))
      (unless (or disabled
                  (if always-ensure
                      (and ensure (eq nil (cadr ensure)))
                    (or (not ensure) (eq nil (cadr ensure)))))
        `(add-to-list 'pinstall-package-list ',pkg))))

  (provide 'use-package)
  (cl-letf (((symbol-function 'package-install)
             (lambda (&rest _) nil))
            ((symbol-function 'package-refresh-contents)
             (lambda (&rest _)) nil))
    (load "~/.emacs.d/init.el"))

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
                     (push pkg no-dep-packages))
                   reqs))
               dep-packages))))
        (setq dep-packages deps)
        (when deps (push deps result))))
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
    (packages-installed-p remained-packages)))

(defun async-install-packages (package-list)
  (let* ((process-number pinstall-process-number)
         (default-args-len (/ (length package-list) process-number))
         (remained-packages package-list)
         (proc-list nil))
    (while remained-packages
      (let* ((args-len (max 1 (min default-args-len (length remained-packages))))
             (packages (mapcar #'symbol-name
                               (cl-subseq remained-packages 0 args-len)))
             (proc (apply
                    #'start-process
                    "Install"
                    nil
                    "emacs" "-batch"
                    "-l" pinstall-file
                    "-install" packages)))
        (push proc proc-list)
        (setq remained-packages (nthcdr args-len remained-packages))))
    (init-process-check package-list proc-list)))

(defun init-function (&rest _)
  (delete "-init" command-line-args)
  (package-archives-init)
  (package-refresh-contents)
  (let* ((deps-list (get-dependency-package-list))
         (packages-list (remove-duplicate-packages-in-depth deps-list))
         (all-packages-count (length (apply #'append packages-list))))
    ;; (print packages-list)
    (if (zerop all-packages-count)
        (message (format "\nAll packages are already installed.\n"))
      (message (format "\n%s packages will be installed.\n" all-packages-count))
      (dolist (packages packages-list)
        (async-install-packages packages)))
    (message "Init done.")))

(defun install-function (&rest _)
  (delete "-install" command-line-args)
  (package-archives-init)
  (dolist (pkg command-line-args-left)
    (package-install (intern pkg) t)))


(add-to-list 'command-switch-alist '("-install" . install-function))
(add-to-list 'command-switch-alist '("-init" . init-function))


;;;###autoload
(defun pinstall-init ()
  (delete "-init" command-line-args)
  (let ((output-buffer (generate-new-buffer "*Init*")))
    (switch-to-buffer output-buffer)
    (call-process "emacs" nil  output-buffer t
                  "-l" pinstall-file "-batch" "-init")
    (package-initialize)))


(provide 'pinstall)
