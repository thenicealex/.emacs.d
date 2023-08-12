;; -*- coding: utf-8; lexical-binding: t; -*-

;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(defvar alex-extra-mode-line-info '()
  "Extra info displayed at the end of the mode line.")

(setq-default mode-line-format
  (list
    ;; the buffer name
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
			'help-echo (buffer-file-name)))

    ;; line and column
    ;; '%02' to set to 2 chars at least; prevents flickering
    "(%02l:%01c) "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    "["
    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))

    ;; buffer file encoding
    '(:eval (let ((sys (coding-system-plist buffer-file-coding-system)))
              (if (memq (plist-get sys :category)
                        '(coding-category-undecided coding-category-utf-8))
                  "UTF-8"
                (upcase (symbol-name (plist-get sys :name))))))
    " "

    ;; insert vs overwrite mode
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face nil
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (and (buffer-modified-p)
                 (propertize "Mod"
                             'face nil
                             'help-echo "Buffer has been modified")))

    ;; is this buffer read-only?
    '(:eval (and buffer-read-only
                 (propertize "RO" 'face nil 'help-echo "Buffer is read-only")))
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh"))))

    '(:eval alex-extra-mode-line-info)

    " %-" ;; fill with '-'
    ))

(set-face-attribute 'mode-line nil
		    :height 0.95
		    :background "SlateGray1")
(set-face-attribute 'mode-line-inactive nil
		    :height 0.95
		    :background "grey93")


(provide 'init-modeline)
