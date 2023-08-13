(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  :bind(:map minibuffer-local-map
	     ("M-h" . backward-kill-word))
  :custom (vertico-cycle t))

;; Configure directory extension with more convenient directory navigation commands
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word) ; Mac Keyboard
              ("C-<backspace>" . vertico-directory-delete-word)) ;; for different keyboard (c-w for all)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)) ; to tidy shadowed file names

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :after vertico
  :ensure t
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :ensure t
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :bind (:map minibuffer-mode-map
           ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind
  ([remap imenu] . #'consult-imenu)
  ;; Minibuffer history
  (:map minibuffer-local-map
  ("C-s" . (lambda ()
	    "Insert the currunt symbol."
	    (interactive)
	    (insert (save-excursion
			(set-buffer (window-buffer (minibuffer-selected-window)))
			(or (thing-at-point 'symbol t) "")))))))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;; Using git submodule
(use-package color-rg
  :load-path "~/.emacs.d/site-lisp/color-rg")

(use-package keycast
  :ensure t
  :commands (alex/toggle-keycast)
  :config
  (defun alex/toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line ""))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON"))))

(use-package which-key
  :ensure t
  :defer 0
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.8))

(use-package avy
  :ensure t)
;; Jump to Chinese characters
(use-package ace-pinyin
  :ensure t
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

(use-package neotree
  :ensure t
  :demand t)

(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; copilot's dependencies
(use-package editorconfig
  :ensure t)
;; Using git submodules
(use-package copilot
  :load-path "~/.emacs.d/site-lisp/copilot.el"
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-l" . 'copilot-accept-completion-by-line)))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pretty-hydra
  :ensure t
  :demand t)

(use-package goto-line-preview
  :ensure t
  :commands (goto-line-preview))

(use-package helpful
  :ensure t
  :hook (help-mode . helpful-mode))

(use-package visual-regexp
  :ensure t
  :commands (vr/replace vr/query-replace))

(use-package kill-ring-search
  :ensure t
  :commands (kill-ring-search))

;; (use-package holo-layer
;;   :load-path "~/.emacs.d/site-lisp/holo-layer"
;;   :init
;;   (setq holo-layer-enable-cursor-animation t)
;;   :config
;;   (holo-layer-enable))
(add-to-list 'load-path "~/.emacs.d/site-lisp/holo-layer")
(require 'holo-layer)
(setq holo-layer-enable-cursor-animation t)
(holo-layer-enable)


;; Git
(use-package magit
  :ensure t
  :commands (magit-status))

(defun magit-submodule-remove-force ()
  "Remove a git submodule with force."
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))

(use-package git-gutter
  :ensure t
  :diminish
  :hook (prog-mode . git-gutter-mode))

(provide 'init-tools)
;;; init-tools.el ends here
