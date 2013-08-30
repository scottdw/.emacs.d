(load "powershell-mode.el")
(load "glsl-mode.el")
(setq solarized-distinct-fringe-background t)
(load-theme 'solarized-dark t)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(require 'window-number)
(window-number-meta-mode 1)

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'remove-dos-eol)

(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(defun nlinum-on ()
  "Turn on `nlinum-mode in current buffer"
  (unless (minibufferp)
    (nlinum-mode 1)))

(define-globalized-minor-mode global-nlinum-mode nlinum-mode nlinum-on)
(global-nlinum-mode)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(setq auto-mode-alist
      (append
       '(
         ("\\.config\\'" . nxml-mode)
         ("\\.targets\\'" . nxml-mode)
         ("\\.ps1\\'" . powershell-mode)
         )
       auto-mode-alist))

(setq diff-command
      "C:/MinGW/msys/1.0/bin/diff")

(setq magit-git-executable
      "C:/Program Files (x86)/Git/bin/git")

(add-to-list 'exec-path "C:/Program Files (x86)/Git/bin/")

(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")

(setq org-src-fontify-natively t)

(setenv "JAVA_HOME" "C:\\Program Files\\Java\\jdk1.7.0_21")
(setenv "JAVA_CMD" "C:\\Program Files\\Java\\jdk1.7.0_21\\bin\\java")

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun format-xml ()
  "Formats a buffer assuming that it contains xml.
For the entire buffer it replaces all whitespace between close and open angle
brackets with a single newline then runs the standard indent-region command on
the buffer."
  (interactive)
  (save-excursion
    (progn
      (replace-regexp ">\\s-*<" ">\n<" nil (point-min) (point-max))
      (indent-region (point-min) (point-max)))))

(defun listify-lines (start end)
  "Replaces a region of lines with the contents of each line separated by a comma and placed within parentheses."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (goto-char end)
      (insert ")")
      (goto-char start)
      (insert "(")
      (replace-regexp "\n" ", " nil start end))))

(defun stringify-lines (start end)
  "Wraps each line of the region in apostrophes."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (replace-regexp "\\s-*\\(.*\\)\\s-*" "'\\1'" nil start end))))

(defun destringify-lines (start end)
  "Removes characters before and including the quote at the start of a line and
  the quote and following characters at the end of the line."
  (interactive "*r")
    (let ((end (copy-marker end)))
      (replace-regexp ".*\"\\(.*\\)\".*" "\\1" nil start end)))

(defun maximize-frame ()
  "Maximizes the frame."
  (interactive "*")
  (w32-send-sys-command 61488))

(defun squash-space (start end)
  "Replaces repeated whitespace with a single space character"
  (interactive "*r")
  (save-excursion)
  (replace-regexp "\\s-+" " " nil start end))

(defun increment-number-at-point (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun qr-encode-region (start end)
  (interactive "r")
  (let ((b "QR-Encode"))
    (kill-buffer (get-buffer-create b))
    (call-process-region start end "java"
                       nil b nil
                       "-cp"
                       "SET CLASSPATH"
                       "InToQrPbm" "-i")
    (switch-to-buffer b)
    (image-mode)))

;; from Bozhidar Batsov
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(define-key global-map "\M-Q" 'unfill-paragraph)

(server-start)