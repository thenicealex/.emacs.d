(use-package evil
  :ensure t
  :defer 1
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (evil-mode)
  ;; https://emacs.stackexchange.com/questions/46371/
  ;; how-can-i-get-ret-to-follow-org-mode-links-when-using-evil-mode
  (with-eval-after-load 'evil-maps
    (evil-define-key '(insert) 'global (kbd "RET") nil))
  :config
  (progn
    (setcdr evil-insert-state-map nil)
    (global-set-key (kbd "C-z") 'evil-normal-state)
    (evil-define-key '(insert) 'global [escape] 'evil-normal-state)
    (evil-define-key '(normal motion) 'global (kbd "f") 'avy-goto-char)
    (evil-define-key '(normal motion) 'global (kbd "s") 'avy-goto-char-2)
    (evil-define-key '(normal motion) 'global (kbd "S") 'avy-resume)
    (evil-define-key '(normal motion) 'lsp-bridge-mode (kbd "g d") 'lsp-bridge-find-def)
    (evil-define-key '(normal motion) 'sort-tab-mode (kbd "[ b") 'sort-tab-select-prev-tab)
    (evil-define-key '(normal motion) 'sort-tab-mode (kbd "] b") 'sort-tab-select-next-tab)
    (evil-define-key 'normal 'lsp-bridge-mode (kbd "g r") 'lsp-bridge-find-references)
    (evil-define-key 'normal 'lsp-bridge-mode (kbd "K") 'lsp-bridge-lookup-documentation)
    (evil-define-key 'normal 'global (kbd "C-r") 'undo-redo)

    ;; Neotree
    (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "a") 'neotree-create-node)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

    (evil-define-key 'normal dired-mode-map
      "h" 'dired-up-directory
      "j" 'dired-next-line
      "k" 'dired-previous-line
      "l" 'dired-find-file
      "q" 'quit-window)

    (evil-define-key '(normal motion) 'global
      "j" "gj"
      "k" "gk")))

;; (use-package undo-tree
;;   :ensure t
;;   :after evil
;;   :init
;;   (global-undo-tree-mode 1)
;;   (setq undo-tree-auto-save-history nil)
;;   (evil-set-undo-system 'undo-tree))

(use-package general
  :ensure t
  :after evil
  :init
  (with-eval-after-load 'evil
    (general-add-hook 'after-init-hook
		      (lambda (&rest _)
			(when-let ((messages-buffer (get-buffer "*Messages*")))
			  (with-current-buffer messages-buffer
			    (evil-normalize-keymaps)))) nil nil t))

  (general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
	 :wrapping global-definer
	 :prefix-map ',(intern (concat "+general-global-" name "-map"))
	 :infix ,infix-key
	 :wk-full-keys nil
	 "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
	,@body)))

  (global-definer
    "!" '(shell-command :which-key "shell command")
    "SPC" 'alex/hightlight-word
    "'" 'vertico-repeat
    "u" 'universal-argument
    "-" 'transient-font-size
    "/" '(comment-or-uncomment :which-key "comment or uncomment")
    "=" '(indent-region :which-key "indent region")
    "e" '(neotree-toggle :which-key "toggle neotree")
    "hf" '(helpful-callable :which-key "describe function")
    "hv" '(helpful-variable :which-key "describe variable")
    "hk" '(helpful-key :which-key "describe key")
    "[" '((lambda () (interactive) (evil-insert-newline-above) (forward-line))
	  :which-key "insert line above")
    "]" '((lambda () (interactive) (evil-insert-newline-below) (forward-line -1))
	  :which-key "insert line below"))

  (+general-global-menu! "search" "s"
    "s" '(consult-line :which-key "search in buffer")
    "r" '(consult-ripgrep :which-key "search in project"))

  (+general-global-menu! "buffer" "b"
    "b" '(consult-buffer :which-key "consult buffer")
    "B" '(switch-to-buffer :which-key "switch buffer")
    "p" '(previous-buffer :which-key "previous buffer")
    "n" '(next-buffer :which-key "next buffer")
    "R" '(alex/rename-this-file :which-key "rename buffer")
    "d" '(kill-current-buffer :which-key "kill buffer")
    "D" '(kill-buffer :which-key "kill buffer")
    "y" '(alex/copy-file-or-buffer-name :which-key "copy buffer name")
    "f" '(my-open-current-directory :which-key "open current directory")
    "m" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
	  :which-key "messages buffer")
    "s" '(scratch-buffer :which-key "scratch buffer"))

  (+general-global-menu! "file" "f"
    "f" '(find-file :which-key "find file")
    "h" '(recentf-open :which-key "recent file")
    "l" '(consult-locate :which-key "locate file")
    "s" '(save-buffer :which-key "save buffer")
    "y" '(alex/copy-file-or-buffer-name :which-key "copy file name")
    "r" '(alex/rename-this-file :which-key "rename file")
    "d" '(alex/delete-this-file :which-key "delete file")
    "m" '(alex/remember-init :which-key "mark current file")
    "b" '(alex/remember-jump :which-key "back marked file")
    "S" '(save-some-buffers :which-key "save all buffers")
    "j" '(dired-jump :which-key "dired jump")
    "!" 'my/exec-shell-on-buffer)

  (+general-global-menu! "window" "w"
    "|" '(split-window-right :which-key "split window right")
    "-" '(split-window-below :which-key "split window below")
    "o" '(delete-other-windows :which-key "delete other windows")
    "u" '(winner-undo :which-key "winner undo")
    "r" '(winner-redo :which-key "winner redo")
    ;; "w" '(ace-window :which-key "select window")
    "s" 'esw/swap-two-windows
    "d" 'esw/delete-window
    "=" 'balance-windows-area
    ;; "r" 'esw/move-window
    "x" 'resize-window
    "H" 'buf-move-left
    "L" 'buf-move-right
    "J" 'buf-move-down
    "K" 'buf-move-up)

  (+general-global-menu! "terminal" "t")

  (+general-global-menu! "git" "g")

  (+general-global-menu! "project" "p"
    "f" 'project-find-file
    "r" 'consult-recent-file
    "s" 'project-find-regexp
    "d" 'project-dired
    "b" 'consult-project-buffer
    "e" 'project-eshell
    "m" 'my/project-run-makefile-target
    "c" 'project-compile
    "t" 'my/project-citre
    "p" 'project-switch-project
    "i" 'my/project-info
    "a" 'project-remember-projects-under
    "x" 'project-forget-project))

(use-package evil-collection
  :ensure t
  :after (evil general)
  :config
  (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))
  (evil-collection-init)

  (cl-loop for (mode . state) in
	   '((org-agenda-mode . normal)
	     (Custom-mode . emacs)
	     (eshell-mode . emacs)
	     (makey-key-mode . motion))
	   do (evil-set-initial-state mode state)))

(use-package evil-surround
  :ensure t
  :hook (evil-mode . evil-surround-mode))

(use-package evil-matchit
  :ensure t
  :hook (evil-local-mode . evil-matchit-mode))

(use-package iedit
  :ensure t
  :defer 2
  :init
  (setq iedit-toggle-key-default nil)
  :config
  (define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
  (define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line))

(use-package evil-multiedit
  :ensure t
  :commands (evil-multiedit-default-keybinds)
  :init
  (evil-multiedit-default-keybinds))

(provide 'init-evil)

