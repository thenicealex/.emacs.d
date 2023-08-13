(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;;; Using git submodule
(use-package yasnippet
  :load-path "~/.emacs.d/site-lisp/yasnippet"
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/yas-snippets")))

;; (add-to-list 'load-path "~/.emacs.d/elisp/lsp-bridge")
;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)
;; (setq acm-enable-quick-access t)
;;; Using git submodule
(use-package lsp-bridge
  :load-path "~/.emacs.d/site-lisp/lsp-bridge"
  :bind (:map lsp-bridge-mode())
  :config
  (setq lsp-bridge-default-mode-hooks
        (remove 'org-mode-hook lsp-bridge-default-mode-hooks))
  (setq lsp-bridge-flash-region-delay 10.0)
  (global-lsp-bridge-mode)
  (setq acm-enable-quick-access t)
  (setq lsp-bridge-enable-search-words t)
  (setq lsp-bridge-enable-completion-in-minibuffer t)
  ;; tooltip的字体大小从默认的130增加到160
  (setq lsp-bridge-lookup-doc-tooltip-font-height 160)
  ;; 关闭tempel可以让补全更快些
  ;; https://emacs-china.org/t/lsp-bridge/20786/2341
  (setq acm-enable-tempel nil)
  ;; (setq acm-backend-search-sdcv-words-dictionary
  ;; 	"d://dictionary//langdao/langdao-ce-gb")
  ;; (setq lsp-bridge-enable-log t)
  )

;;; --- blove/korean-zh-dict
(defun toggle-dict-kc ()
  "switch to korean-zh dict."
  (interactive)
  (setq acm-backend-search-sdcv-words-dictionary (expand-file-name "naver_krcn/Naver_KrCn" user-emacs-directory))
  (lsp-bridge-restart-process)
  )
;;; --- blove/zh-korean-dict
(defun toggle-dict-ck ()
  "switch to zh-korean dict."
  (interactive)
  (setq acm-backend-search-sdcv-words-dictionary (expand-file-name "naver_cnkr/Naver_CnKr" user-emacs-directory))
  (lsp-bridge-restart-process)
  )

;; (use-package flycheck
;;   :ensure t
;;   :hook (prog-mode . flycheck-mode))


(provide 'init-lsp)
;;; init-lsp.el ends here
