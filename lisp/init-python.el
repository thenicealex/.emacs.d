;; https://github.com/manateelazycat/lsp-bridge
(setq lsp-bridge-python-command "C:\\Users\\liran\\scoop\\apps\\python\\3.11.4\\python.exe")

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :hook ((python-ts-mode python-mode) . elpy-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ; 禁用使用 TAB 键插入制表符
            (setq tab-width 4))) ; 设置 TAB 键的宽度为 4 个空格


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ; 禁用使用 TAB 键插入制表符
            (setq tab-width 2))) ; 设置 TAB 键的宽度为 4 个空格

(provide 'init-python)
