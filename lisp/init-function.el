;;; My funtion in here.

;;
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)


(defun alex/hightlight-word (word)
  (interactive "sEnter the word: ")
  (defface blove-rg-match-hl-face
    '((t (:foreground "#FF0000" :bold t)))
    "for rg-match-hl")
  (remove-overlays (point-min) (point-max) 'face 'blove-rg-match-hl-face)
  (save-excursion (beginning-of-buffer)
	 ;; 用 `while` + `re-search-forward` 循环查找，
	 ;; 用 `match-beginning` / `match-end` 确定匹配目标的边界。
	 (while (re-search-forward word nil t)
	   (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
	     (overlay-put ov 'face 'blove-rg-match-hl-face)))))

;;
(defun consult-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fopen externally: ")
  (if (and (eq system-type 'windows-nt)
	   (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
	    (format "start %s" (file-name-directory (expand-file-name file)))) 'gbk))))
(require 'embark)
(define-key embark-file-map (kbd "E") #'consult-directory-externally)

(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))

;; Search and replace
(eval-after-load 'consult
  '(eval-after-load 'embark
     '(progn
	      (require 'embark-consult)
	      (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))

(defun embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                       (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
               (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                           (embark-export)))
      (x (user-error "embark category %S doesn't support writable export" x)))))

(define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write)

;; everyting
;; consult-locate
;; install es.exe and add it to path in windows
(if (eq system-type 'windows-nt)
    (progn
      (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
      (add-to-list 'process-coding-system-alist '("[rR][gG]" . (utf-8-dos . windows-1251-dos)))
      (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))
(eval-after-load 'consult
  (progn
      (setq consult-narrow-key "<"
	      consult-line-numbers-widen t
	      consult-async-min-input 2
	      consult-async-refresh-delay  0.15
	      consult-async-input-throttle 0.2
	      consult-async-input-debounce 0.1)))

(defun transient-winner-undo ()
  "Transient version of `winner-undo'."
  (interactive)
  (let ((echo-keystrokes nil))
    (winner-undo)
    (message "Winner: [u]ndo [r]edo")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?u] #'winner-undo)
       (define-key map [?r] #'winner-redo)
       map)
     t)))

(defun transient-font-size ()
  "Transient version of font-size."
  (interactive)
  (let ((echo-keystrokes nil))
    (message "Adjust the font size: [i]Increase [d]Decrease")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?i] #'text-scale-increase)
       (define-key map [?d] #'text-scale-decrease)
       map)
     t)))

;; 修改自 https://www.emacswiki.org/emacs/DiredOmitMode
(define-advice dired-do-print (:override (&optional _))
  "Show/hide dotfiles."
  (interactive)
  (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
      (progn
        (setq-local dired-dotfiles-show-p nil)
        (dired-mark-files-regexp "^\\.")
        (dired-do-kill-lines))
    (revert-buffer)
    (setq-local dired-dotfiles-show-p t)))


(defun alex/current-minor-modes ()
  "Return the list of minor modes enabled in the current buffer."
  (interactive)
  (delq nil
        (mapcar (lambda (mode)
                  (if (and (boundp mode) (symbol-value mode))
                      mode))
                minor-mode-list)))

(defun alex/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun alex/rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun alex/copy-file-or-buffer-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
	(buffername (buffer-name)))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied filename '%s'" filename))
      (kill-new buffername)
      (message "Copied buffername '%s'" buffername))))

(defun alex/reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))

(defun alex/treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

;; `seq-sort-by', added at Emacs 26.1
(require 'seq)


;; Refer to https://emacs-china.org/t/elpa/11192
(defun alex/test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.

Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((durations (mapcar
                     (lambda (pair)
                       (let ((url (concat (cdr (nth 2 (cdr pair)))
                                          "archive-contents"))
                             (start (current-time)))
                         (message "Fetching %s..." url)
                         (ignore-errors
                           (url-copy-file url null-device t))
                         (float-time (time-subtract (current-time) start))))
                     alex-package-archives-alist))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            alex-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'vertical
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (p) (symbol-name (car p))) alex-package-archives-alist)
       "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "`%s' is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))


;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Now HTTP proxy is `%s'" alex-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,alex-proxy)
          ("https" . ,alex-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun alex/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun alex/remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun alex/remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun alex/count-words ()
  "Count the number of word in buffer, include Chinese."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end)))
    (count-ce-words begin end)))

(defun count-ce-words (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (CN: %d, EN: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

(defun alex/check-package-in-melpa (package)
  "Check if a package exists in MELPA."
  (interactive "sEnter package name: ")
  (unless (package-installed-p package)
    (package-refresh-contents)
    (unless (assoc package package-archive-contents)
      (message "Package '%s' not found in MELPA." package)))
  (message "Package '%s' is installed." package))


(provide 'init-function)
;;; init-function.el ends here
