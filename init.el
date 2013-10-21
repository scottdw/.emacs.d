;;; init.el --- Personal GNU/Emacs initialisation file.
;;; Commentary:
;;; Initialisation file that should be shareable across installations,
;;; ie. no machine specific settings.
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(set-scroll-bar-mode nil)

(let ((font-string "DejaVu Sans Mono-10"))
  (when (x-list-fonts font-string)
    (set-face-attribute 'default nil :font font-string)))

(defface org-block-background
  '((t (:background "#f3f3f3")))
  "Face used for the source block background.")

(add-to-list 'load-path "~/elisp")

(when (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

(when (require 'edit-server)
  (edit-server-start)
  (setq edit-server-new-frame nil))

(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq initial-buffer-choice t)

(semantic-mode 1)

(require 'auto-complete)
(add-to-list 'ac-modes 'clojure-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)

(require 'whitespace)
(setq whitespace-style '(face tabs trailing space-before-tab empty space-after-tab))
(global-whitespace-mode)

(smex-initialize)
;; smex bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; iswitchb
(iswitchb-mode t)

;; Also highlight parens
(show-paren-mode 1)

(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load "flycheck"
  '(progn
     (define-key flycheck-mode-map (kbd "C-c C-n") 'flycheck-next-error)
     (define-key flycheck-mode-map (kbd "C-c C-p") 'flycheck-previous-error)))

(global-set-key (kbd "C-c C-f") 'find-file)
(global-set-key "\M-g" 'goto-line)

(load "cc-mode")
(global-set-key (kbd "C-<delete>") 'c-hungry-delete-forward)
(global-set-key (kbd "C-<backspace>") 'c-hungry-delete-backwards)

(display-time)

(setq initial-scratch-message
      (format ";; scratch buffer created %s\n;; happy hacking with GNU Emacs %s\n\n" (format-time-string "%Y-%m-%d at %T") emacs-version))

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))

(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(load-file "~/.emacs.d/local.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#002b36")
 '(background-mode dark)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(current-language-environment "UTF-8")
 '(cursor-color "#839496")
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "be7eadb2971d1057396c20e2eebaa08ec4bfd1efe9382c12917c6fe24352b7c1" default)))
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(foreground-color "#839496")
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(show-paren-mode t)
 '(tab-always-indent (quote complete))
 '(truncate-lines t)
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.3 :family "DejaVu"))))
 '(variable-pitch ((t (:family "DejaVu")))))
(put 'downcase-region 'disabled nil)
(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'upcase-region 'disabled nil)

(find-file "~/.emacs.d/init.el")
(find-file "~/.emacs.d/local.el")

;;; init.el ends here
