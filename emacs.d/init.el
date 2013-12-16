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

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Graphene
(require 'graphene)

;; Look and Feel
(load-theme 'monokai t)
(column-number-mode t)

;; Magit
(require 'magit)

;; Pianobar
(load "pianobar-conf.el")

;; OrgMode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Zeal-At-Point
(require 'zeal-at-point)
(global-set-key "\C-cs" 'zeal-at-point)

;; Language Settings

;;; C/C++
(setq c-default-style "linux"
      c-basic-offset 4)

;;;; CMake
(defun maybe-cmake-project-hook ()
  "Function to determine if projects use CMake or regular Make."
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

;;; Elixir Modes
(require 'elixir-mode)

;;; Go Modes
(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)

;;; Haskell Modes
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(define-key haskell-mode-map "\C-c h" 'haskell-hoogle)
(add-hook 'haskell-mode-hook (lambda ()
			       (setf haskell-program-name "ghci")))


;;; Lisp (Common)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup)
(define-key slime-editing-map (kbd "\C-c m") 'slime-eval-last-expression)

;;; Lisp (Clojure)
(require 'clojure-mode)

;;; Markdown Modes
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (when buffer-file-name
	      (add-hook 'after-save-hook
			'check-parens nil t))))

;;; Python Modes
(require 'elpy)

(provide 'init)
;;; init.el ends here
