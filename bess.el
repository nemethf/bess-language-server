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

