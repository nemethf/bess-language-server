;;; emacs configuration ideas, append this to your Emacs init file.

(setenv "BESS" "/opt/bess")

(define-derived-mode bess-mode python-mode "bess"
  (setq-local eldoc-message-function 'bess-eldoc-minibuffer-message))
(add-to-list 'auto-mode-alist '("\\.bess\\'" . bess-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(bess-mode . ("bessls"))))

(require 'subr-x)
(defun bess-eldoc-minibuffer-message (format-string &rest args)
  "Display the head of messages.
Otherwise work like `eldoc-minibuffer-message'."
  (if (minibufferp)
      (progn
	(add-hook 'minibuffer-exit-hook
		  (lambda () (setq eldoc-mode-line-string nil
			      ;; https://debbugs.gnu.org/16920
			      eldoc-last-message nil))
		  nil t)
	(with-current-buffer
	    (window-buffer
	     (or (window-in-direction 'above (minibuffer-window))
		 (minibuffer-selected-window)
		 (get-largest-window)))
          (when mode-line-format
            (unless (and (listp mode-line-format)
                         (assq 'eldoc-mode-line-string mode-line-format))
              (setq mode-line-format
                    (list "" '(eldoc-mode-line-string
                               (" " eldoc-mode-line-string " "))
                          mode-line-format))))
          (setq eldoc-mode-line-string
                (when (stringp format-string)
                  (apply #'format-message format-string args)))
          (force-mode-line-update)))
    (let ((msg (apply 'format-message (or format-string "") args))
          (ea-width (window-width (minibuffer-window)))
          (rows-left (if (floatp max-mini-window-height)
                         (floor (* max-mini-window-height (frame-height)))
                       max-mini-window-height))
          (lines))
      (dolist (line (split-string msg "\n"))
	(let ((chars-left (1- (* rows-left ea-width)))
              (num-rows (1+ (floor (/ (length line) ea-width)))))
	  (when (< 0 rows-left)
            (setq lines (cons (substring line 0 (min (length line) chars-left))
                              lines))
            (setq rows-left (- rows-left num-rows)))))
      (message "%s" (or (string-join (nreverse lines) "\n") "")))))


;; Find file at point with line numbers
;; https://www.emacswiki.org/emacs/FindFileAtPoint#toc6
;; -- DanielPoersch
(ffap-bindings)
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
		(error nil))
              string))
         (line-number-string
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (goto-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))

