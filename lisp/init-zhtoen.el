;;;
;;; blove-lang-helper.el
;;;
(defalias 'lsp-bridge-toggle-korean-helper #'acm-toggle-korean-helper)
(defalias 'lsp-bridge-toggle-us-eng-helper #'acm-toggle-us-eng-helper)

(defvar-local acm-enable-korean-helper nil)
(defvar-local acm-enable-us-eng-helper nil)

(defcustom acm-backend-korean-min-length 1
  "Minimum length of korean word."
  :type 'integer)
(defcustom acm-backend-us-eng-min-length 1
  "Minimum length of us english word."
  :type 'integer)

;; (setq acm-icon-alist (append acm-icon-alist '(("korean" "material" "korean" "#ed6856"))))
(defun acm-backend-language-candidates (min-length keyword dict)
  (let* ((candidates (list)))
    (when (>= (length keyword) min-length)
      (dolist (candidate dict)
        (when (string-prefix-p (downcase keyword) candidate)
          (add-to-list 'candidates (list :key candidate
                                         :icon "translate"
                                         :label candidate
                                         :display-label candidate
                                         :annotation (get-text-property 0 :initials candidate)
                                         :backend "blove-lang")
                       t))))

    candidates))

(defun acm-toggle-korean-helper ()
  "Toggle korean helper."
  (interactive)
  (setq-local acm-enable-us-eng-helper nil)
  (if acm-enable-korean-helper
      (message "Turn off korean helper.")
    (message "Turn on korean helper."))
  (setq-local acm-enable-korean-helper (not acm-enable-korean-helper))
  )
(defun acm-toggle-us-eng-helper ()
  "Toggle us english helper."
  (interactive)
  (setq-local acm-enable-korean-helper nil)
  (if acm-enable-us-eng-helper
      (message "Turn off us english helper.")
    (message "Turn on us english helper."))
  (setq-local acm-enable-us-eng-helper (not acm-enable-us-eng-helper))
  )

(add-to-list 'load-path (concat user-emacs-directory "lisp/blove-extra/blove-lang-helper/blove-lang-zh"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/blove-extra/blove-lang-helper/blove-zh-lang"))
(defun acm-update-candidates-advice ()
  (let* ((keyword (acm-get-input-prefix))
         (char-before-keyword (save-excursion
                                (backward-char (length keyword))
                                (acm-char-before)))
         (candidates (list))
         path-candidates
         yas-candidates
         tempel-candidates
         mode-candidates)

	;; --- add language-helper for korean-helper / us-eng-helper --- begin
	(if (or acm-enable-korean-helper acm-enable-us-eng-helper)
		(progn
		  (setq-local fin-dict '())
		  (if acm-enable-korean-helper
			  (progn
				(require 'blove-backends-kor-zh-15000)
				(require 'blove-backends-zh-kor-15000)
				(setq fin-dict (append fin-dict blove-kor-zh-15000))
				(setq fin-dict (append fin-dict blove-zh-kor-15000))
				(setq candidates (acm-backend-language-candidates acm-backend-korean-min-length keyword fin-dict))
				)
			(progn
			  (if acm-enable-us-eng-helper
				  (progn
					(require 'blove-backends-us-zh-15000)
					(require 'blove-backends-zh-us-15000)
					(setq fin-dict (append fin-dict blove-us-zh-15000))
					(setq fin-dict (append fin-dict blove-zh-us-15000))
					(setq candidates (acm-backend-language-candidates acm-backend-us-eng-min-length keyword fin-dict))
					)
				)
			  )
			)
		  )
	  ;; --- add language-helper for korean-helper / us-eng-helper --- end
	  (progn
		;; from acm.el :: acm-update-candidates --- begin
		(if acm-enable-english-helper
			;; Completion english if option `acm-enable-english-helper' is enable.
			(progn
			  (require 'acm-backend-english-data)
			  (require 'acm-backend-english)

			  (setq candidates (acm-backend-english-candidates keyword)))

		  (setq path-candidates (acm-backend-path-candidates keyword))
		  (if (> (length path-candidates) 0)
			  ;; Only show path candidates if prefix is valid path.
			  (setq candidates path-candidates)

			;; Fetch syntax completion candidates.
			(setq mode-candidates (append
								   (acm-backend-elisp-candidates keyword)
								   (acm-backend-lsp-candidates keyword)))

			;; Don't search snippet if char before keyword is not in `acm-backend-lsp-completion-trigger-characters'.
			(unless (member char-before-keyword acm-backend-lsp-completion-trigger-characters)
			  (setq yas-candidates (acm-backend-yas-candidates keyword))
			  (setq tempel-candidates (acm-backend-tempel-candidates keyword)))

			;; Insert snippet candidates in first page of menu.
			(setq candidates
				  (if (> (length mode-candidates) acm-snippet-insert-index)
					  (append (cl-subseq mode-candidates 0 acm-snippet-insert-index)
							  yas-candidates
							  tempel-candidates
							  (cl-subseq mode-candidates acm-snippet-insert-index))
					(append mode-candidates yas-candidates tempel-candidates)
					))))
		;; from acm.el :: acm-update-candidates --- end
		)
	  )
    candidates))

(advice-add 'acm-update-candidates :override #'acm-update-candidates-advice)

;;; ========== at-the-bottom
(provide 'init-zhtoen)
