(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(set-scroll-bar-mode 'right)

(let ((font-string "DejaVu Sans Mono-10"))
  (when (x-list-fonts font-string)
    (set-face-attribute 'default nil :font font-string)))

(add-to-list 'load-path "~/elisp")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(setq browse-url-browser-function 'browse-url-firefox)
(setq column-number-mode 1)
(setq confirm-kill-emacs 'yes-or-no-p)
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
(setq-default indent-tabs-mode nil)

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

(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(global-set-key (kbd "C-c C-f") 'find-file)
(global-set-key "\M-g" 'goto-line)

(load "cc-mode")
(global-set-key (kbd "C-<delete>") 'c-hungry-delete-forward)
(global-set-key (kbd "C-<backspace>") 'c-hungry-delete-backwards)

(display-time)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
