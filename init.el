;;; init.el --- Personal GNU/Emacs initialisation file.
;;; Commentary:
;;; Initialisation file that should be shareable across installations,
;;; ie. no machine specific settings.
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; These properties must be set before loading the theme.
(require 'solarized)
(setq
 solarized-distinct-fringe-background t
 solarized-use-variable-pitch nil
 solarized-scale-org-headlines nil
 solarized-use-more-italic t
 solarized-height-minus-1 1.0
 solarized-height-plus-1 1.0
 solarized-height-plus-2 1.0
 solarized-height-plus-3 1.0
 solarized-height-plus-4 1.0)
(load-theme 'solarized-dark t)

(require 'smart-mode-line)
(setq sml/theme nil)
(sml/setup)

(let ((font-string "DejaVu Sans Mono-10"))
  (when (x-list-fonts font-string)
    (set-face-attribute 'default nil :font font-string)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Configure smartparens
(require 'smartparens-config)

(add-to-list 'load-path "~/elisp")

;; Keybindings

;; hungry-delete
(global-set-key (kbd "C-<delete>") #'hungry-delete-forward)
(global-set-key (kbd "C-<backspace>") #'hungry-delete-backward)

;; smex
(global-set-key (kbd "M-x") #'smex)
(global-set-key (kbd "M-X") #'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)

(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  t)

(defun initialise-global-modes ()
  "Turn on global modes."
  (global-company-mode 1)
  (global-flycheck-mode 1)
  (global-whitespace-mode 1)
  (global-diff-hl-mode 1)
  (global-prettify-symbols-mode 1)
  (icomplete-mode 1)
  (projectile-global-mode 1)
  (semantic-mode 1)
  (smartparens-global-mode 1)
  (smex-initialize)
  (window-number-meta-mode)
  (window-number-mode)
  (server-start)
  (edit-server-start))

(defun initialise-clj-refactor ()
  "Enables `clj-refactor-mode' and its keybindings."
  (clj-refactor-mode 1)
  (yas/minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c r"))

(add-hook 'after-init-hook #'initialise-global-modes)
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'remove-dos-eol)
(add-hook 'cider-repl-mode-hook #'initialise-clj-refactor)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'initialise-clj-refactor)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'comint-output-filter-functions #'comint-watch-for-password-prompt)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
;; warm artifact cache at REPL start up
(add-hook 'cider-connected-hook #'cljr-update-artifact-cache)
;; warm the AST cache at REPL start up
(add-hook 'cider-connected-hook #'cljr-warm-ast-cache)
(add-hook 'smartparens-mode #'sp-use-smartparens-bindings)
(add-hook 'restclient-response-loaded-hook #'view-mode)

(eval-after-load 'flycheck
  '(progn
     (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
     (flycheck-clojure-setup)
     (define-key flycheck-mode-map (kbd "C-c C-n") #'flycheck-next-error)
     (define-key flycheck-mode-map (kbd "C-c C-p") #'flycheck-previous-error)))

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-capf)))

(display-time)

(setq initial-scratch-message
      (format ";; scratch buffer created %s\n;; happy hacking with GNU Emacs %s\n\n" (format-time-string "%Y-%m-%d at %T") emacs-version))

(setq org-startup-folded "showall")

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
 '(calendar-date-style (quote iso))
 '(cider-auto-select-error-buffer t)
 '(cider-repl-use-clojure-font-lock t)
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
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(make-backup-files nil)
 '(restclient-log-request nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sp-hybrid-kill-entire-symbol nil)
 '(tab-always-indent (quote complete))
 '(truncate-lines t)
 '(visible-bell t)
 '(whitespace-style (quote (face tabs trailing space-before-tab empty space-after-tab))))

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

(find-file "~/.emacs.d/local.el")
(find-file "~/.emacs.d/init.el")

;;; init.el ends here
