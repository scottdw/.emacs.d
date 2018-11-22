;;; init.el --- Personal GNU/Emacs initialisation file.
;;; Commentary:
;;; Initialisation file that should be shareable across installations,
;;; ie. no machine specific settings.
;;; Code:

(eval-after-load "package"
  '(progn
     (add-to-list 'package-archives
                  '("melpa" . "http://melpa.org/packages/") t)))

(add-to-list 'load-path "~/elisp")

(package-initialize)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(require 'use-package)
(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0))

(load-theme 'solarized-dark t)

(let ((font-string "DejaVu Sans Mono-10"))
  (when (x-list-fonts font-string)
    (set-face-attribute 'default nil :font font-string)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Configure smartparens
(use-package smartparens
  :ensure t
  :config (require 'smartparens-config)
  :hook sp-use-smartparens-bindings)

(use-package hungry-delete
  :ensure t
  :bind
  (("C-<delete>" . hungry-delete-forward)
   ("C-<backspace>" . hungry-delete-backward)))

(use-package aggressive-indent
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase nil)
  (add-to-list 'company-backends 'company-capf))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package flycheck-clojure
  :ensure t
  :after (flycheck cider)
  :config
  (flycheck-clojure-setup))

(use-package diff-hl
  :ensure t)

(use-package edit-server
  :ensure t)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
;;  (add-hook 'cider-repl-mode-hook #'remove-dos-eol)
)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  )

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :config (setq restclient-log-request nil))

(use-package ws-butler
  :ensure t)

(use-package bln-mode
  :ensure t
  :bind ("M-j" . hydra-bln/body)
  :init
  (defhydra hydra-bln (:hint none)
     "
Binary Search Navigation:
Line  : _j_, _k_
Buffer: _h_, _l_
Window: _u_, _i_
Quit: _q_"
     ("j" bln-backward-half "Backward in line")
     ("k" bln-forward-half "Forward in line")
     ("u" bln-backward-half-v "Backward in window")
     ("i" bln-forward-half-v "Forward in window")
     ("h" bln-backward-half-b "Backward in buffer")
     ("l" bln-forward-half-b "Forward in buffer")
     ("q" (message "Abort") :exit t)))

(defun initialise-global-modes ()
  "Turn on global modes."
  (global-company-mode 1)
  (global-whitespace-mode 1)
  (global-diff-hl-mode 1)
  (global-prettify-symbols-mode 1)
  (ws-butler-global-mode 1)
  (icomplete-mode 1)
  (projectile-mode 1)
  (semantic-mode 1)
  (smartparens-global-mode 1)
  (server-start)
  (edit-server-start))

(add-hook 'after-init-hook #'initialise-global-modes)

(add-hook 'comint-output-filter-functions #'comint-watch-for-password-prompt)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

(display-time)

(setq initial-scratch-message
      (format ";; scratch buffer created %s\n;; happy hacking with GNU Emacs %s\n\n" (format-time-string "%Y-%m-%d at %T") emacs-version))

(setq org-startup-folded "showall")

(add-hook 'org-mode-hook (lambda () (org-display-inline-images t t)))

(defvar user-temporary-file-directory
  (concat temporary-file-directory "emacs/"))

(make-directory user-temporary-file-directory t)
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))
(setq create-lockfiles nil)

(load-file "~/.emacs.d/local.el")

(find-file "~/.emacs.d/local.el")
(find-file "~/.emacs.d/init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#002b36")
 '(background-mode dark)
 '(calendar-date-style (quote iso))
 '(cider-auto-select-error-buffer t)
 '(cider-repl-use-clojure-font-lock t)
 '(cider-repl-use-pretty-printing t)
 '(cider-show-error-buffer t)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(current-language-environment "UTF-8")
 '(cursor-color "#839496")
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "be7eadb2971d1057396c20e2eebaa08ec4bfd1efe9382c12917c6fe24352b7c1" default)))
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(edit-server-new-frame nil)
 '(foreground-color "#839496")
 '(help-window-select t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(make-backup-files nil)
 '(mode-require-final-newline nil)
 '(nxml-sexp-element-flag t)
 '(restclient-log-request nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sp-hybrid-kill-entire-symbol nil)
 '(tab-always-indent (quote complete))
 '(truncate-lines t)
 '(visible-bell t)
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab empty space-after-tab))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)
(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; init.el ends here
