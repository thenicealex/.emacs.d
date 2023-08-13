(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil)
  (setq doom-modeline-minor-modes nil
	doom-modeline-indent-info t)
  :custom-face
  ;; Set colors to distinguish between active and inactive windows
  (mode-line ((t (:height 0.95 :background "SlateGray1"))))
  (mode-line-inactive ((t (:height 0.95 :background "grey93")))))

(provide 'init-ui)
