(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(set-scroll-bar-mode 'right)

(add-to-list 'load-path "~/elisp")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(setq browse-url-browser-function 'browse-url-firefox)
(setq column-number-mode 1)
(setq default-truncate-lines 1)
(setq display-time-24hr-format t)
(setq display-time-mode t)
(setq european-calendar-style t)
(setq inhibit-startup-message t)
(setq make-backup-files nil) 
(setq show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq tab-always-indent 'complete)
(setq visible-bell t)
(setq whitespace-style '(trailing space-before-tab indentation space-after-tab))
(setq-default indent-tabs-mode nil)

(add-to-list 'ac-modes 'clojure-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(semantic-mode 1)

(smex-initialize)

;; iswitchb
(iswitchb-mode t)

;; Also highlight parens
(show-paren-mode 1)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook (lambda () (whitespace-mode 1)))

(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; smex bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key "\M-g" 'goto-line)

(load "cc-mode")
(global-set-key (kbd "C-<delete>") 'c-hungry-delete-forward)
(global-set-key (kbd "C-<backspace>") 'c-hungry-delete-backwards)

(display-time)
(add-hook 'diary-hook 'appt-make-list)
(diary 0)

(put 'downcase-region 'disabled nil)

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
