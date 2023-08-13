;; -*- coding: utf-8; lexical-binding: t; -*-
;; Startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
              (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
              gcs-done)))

;; set gc-threshold after init
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold most-positive-fixnum)))

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq user-lisp-directory (concat user-emacs-directory "lisp"))

(defvar alex-debug nil "Enable debug mode.")

(setq ISMAC (eq system-type 'darwin))
(setq ISWIN (eq system-type 'windows-nt))
(setq ISCYGWIN (eq system-type 'cygwin) )
(setq ISLINUX (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq EMACS28 (>= emacs-major-version 28))
(setq-default tab-width 4) 

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
(unless (bound-and-true-p package--initialized)
  (package-initialize))
;; Setup 'use-package'
(when (version< emacs-version "29.0")
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Load the settings recorded through Emacs
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (if ISWIN
      (shell-command (concat "type " custom-file))
    (shell-command (concat "touch " custom-file))))
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

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

(add-to-list 'load-path user-lisp-directory)
(require 'init-custom)
(require 'init-fonts)
(require 'eldoc-extension)
(require 'init-evil)
(require 'init-ui)
(require 'init-tools)
(require 'init-function)
(require 'init-builtin)
(require 'init-lsp)
(require 'init-python)
(require 'init-org)
;;; init.el ends here
