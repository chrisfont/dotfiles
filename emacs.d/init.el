;;; init.el --- Summary
;;; Author: Chris Font
;;; Commentary:
;;;; This is my person Emacs init file.  It requires an additional packages file
;;;; as well as a pianobar configuration file.  If you are using my init from my
;;;; Github, you will have received this dependencies as well.

;;; Code:

;; General Config
(add-to-list 'load-path "~/.emacs.d/")

;; Elpa Config
(load "packages.el")

(require 'sublimity)
(require 'sublimity-scroll)
(sublimity-mode t)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Backup
(setq backup-by-copying      t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions    t
      kept-new-versions      6
      kept-old-versions      2
      version-control        t)

;; Look and Feel
(global-font-lock-mode t)

(set-frame-parameter nil                   'font "Consolas-10")
(setq                inhibit-splash-screen t)

(tool-bar-mode       -1)

(load-theme          'monokai              t)
(column-number-mode  t)
(global-linum-mode   t)
(global-hl-line-mode t)

(setq-default indent-tabs-mode  nil)

(setq scroll-margin             5)
(setq scroll-conservatively     1)
(setq scroll-down-aggressively 10)

(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere           t)
(ido-mode     t)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)

;;; Speedbar
(require 'speedbar)
(speedbar-add-supported-extension ".hs")

;; Magit
(require 'magit)

;; Pianobar
(load "pianobar-conf.el")

;; OrgMode
(require 'org-install)
(add-to-list           'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl"          'org-store-link)
(define-key global-map "\C-ca"          'org-agenda)
(setq-default          org-log-done     t)

;; Smex
(smex-initialize)
(global-set-key (kbd "M-x")         'smex)
(global-set-key (kbd "M-x")         'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Language Settings

;;; C/C++
(setq-default c-default-style  "linux"
              c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;;;; CMake
(defun maybe-cmake-project-hook ()
  "Function to determine if projects use CMake or regular Make."
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))

(add-hook 'c-mode-hook   'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

;;;; Clojure
(require 'clojure-mode)
(require 'kibit-mode)
(add-hook 'clojure-mode-hook 'kibit-mode)
(add-hook 'clojure-mode-hook 'flymake-mode-on)

;;;; GGTags
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode)
	      (ggtags-mode t))))

;;; Elixir Modes
(require 'elixir-mode)

;;; Go Modes
(require 'go-mode-load)
(require 'go-errcheck-autoloads)
;;(require 'go-autocomplete)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c i") 'go-goto-imports)))
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c C-i") 'go-errcheck)))

;;; Groovy Modes
(add-to-list 'auto-mode-alist        '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy"    . groovy-mode))

;;; Haskell Modes
(require 'haskell-mode)
(require 'tidal)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(define-key haskell-mode-map "\C-c h" 'haskell-hoogle)
(add-hook 'haskell-mode-hook (lambda ()
			       (setf haskell-program-name "ghci")))


;;; Lisp (Common)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup)
(define-key slime-editing-map (kbd "\C-c m") 'slime-eval-last-expression)

(setq common-lisp-hyperspec-root "file:///home/chris/.hyperspec/")

(defun ddp-lispdoc ()
  "Search lispdoc.com for symbol under cursor."
  (interactive)
  (let* ((symbol-at-point (symbol-at-point))
	 (thing (symbol-name symbol-at-point)))
    (browse-url (concat "http://lispdoc.com?q=" thing "&search=basic+search"))))


;;; Markdown Modes
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (when buffer-file-name
	      (add-hook 'after-save-hook
			'check-parens nil t))))

;;; Python Modes
(require 'elpy)

;;;; SConstruct
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

;;;; AUTOCOMPLETE
(add-hook 'after-init-hook 'global-auto-complete-mode nil)
(add-hook 'after-init-hook 'global-company-mode)


;;; Scheme/Racket
(require 'quack)
(eval-after-load 'scheme
  '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete))

(setq lisp-indent-function 'scheme-smart-indent-function)

(provide 'init)
;;; init.el ends here
