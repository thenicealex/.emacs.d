(use-package all-the-icons
  :ensure t)

(defun nasy/orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))
(defun nasy/setup-corfu ()
  "Setup corfu."
  (setq-local orderless-matching-styles '(orderless-flex)
              orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'nasy/orderless-dispatch-flex-first nil 'local))
(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-min-width 30)
  (corfu-max-width 100)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-echo-documentation t)
  :bind (:map corfu-map
	      ("C-d" . corfu-info-documentation)
	      ("M-." . corfu-info-location))
  :hook (prog-mode . nasy/setup-corfu))
  ;; :init (global-corfu-mode))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
    ("C-M-/" . dabbrev-expand)))

(provide 'init-completion)
