;; -*- coding: utf-8; lexical-binding: t; -*-
;; (setq gc-cons-percentage 0.6)
;; (setq gc-cons-threshold most-positive-fixnum)

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(defvar alex-debug nil "Enable debug mode.")

(setq ISMAC (eq system-type 'darwin))
(setq ISWIN (eq system-type 'windows-nt))
(setq ISCYGWIN (eq system-type 'cygwin) )
(setq ISLINUX (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq EMACS28 (>= emacs-major-version 28))

(defconst alex-emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d.")

(defconst alex-site-lisp-dir (concat alex-emacs-d "site-lisp")
  "Directory of site-lisp.")

(defconst alex-lisp-dir (concat alex-emacs-d "lisp")
  "Directory of personal configuration.")

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; Show full path in the title bar.
(setq-default frame-title-format "%b (%f)")
;; hide title bar
;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

;;; --- Encoding
;;;
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq xref-history-storage 'xref-window-local-history)
;;; ~ fix qindos font view for windows only ~
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:\\Program Files\\PowerShell\\7\\pwsh.exe")
  (setq explicit-powershell.exe-args '())
  (setq default-directory "D:/workstation/")
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)

(defun alex-add-subdirs-to-load-path (lisp-dir)
  "Add sub-directories under LISP-DIR into `load-path'."
  (let* ((default-directory lisp-dir))
    (setq load-path
          (append
           (delq nil
                 (mapcar (lambda (dir)
                           (unless (string-match "^\\." dir)
                             (expand-file-name dir)))
                         (directory-files lisp-dir)))
           load-path))))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-fonts)
(require 'init-custom)
(require 'eldoc-extension)
(require 'init-evil)
(require 'init-ui)
(require 'init-tools)
(require 'init-function)
(require 'init-builtin)
(require 'init-lsp)
(require 'init-python)



(message "*** Emacs loaded in %s with %d garbage collections."
  (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done)
;;; init.el ends here
