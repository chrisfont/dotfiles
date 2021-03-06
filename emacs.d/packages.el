
;; Packages.el --- Summary
;; Emacs Configuration file

;;; Commentary:
;; This file provides settings for ELPA packages
;; for my Emacs configuration, including whether or not
;; packages are required.

;;; Code:
(require 'cl)

;; ELPA Repos
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa"     . "http://melpa.milkbox.net/packages/")))



;; Finally, finish package init and return
(package-initialize)

;; Required Package Lists
(defvar *required-packages*
  '(clojure-mode company company-go elixir-mode elpy flycheck flycheck-d-unittest flycheck-color-mode-line flycheck-tip flycheck-ledger geiser go-mode go-eldoc go-errcheck haskell-mode lua-mode kibit-mode magit markdown-mode monokai-theme org quack pianobar python-mode pyflakes rainbow-delimiters slime smartparens smex))


;; Package auto-installation code -- Thanks bbatsov
(defun needed-packages-installed-p ()
  "Loops over *required-packages* to check if they are installed."
  (loop for package in *required-packages*
	when (not (package-installed-p package)) do (return nil)
	finally (return t)))

(unless (needed-packages-installed-p)
  (message "%s" "Package database being refreshed, please wait...")
  (package-refresh-contents)
  (message "%s" "done.")
  (dolist (package *required-packages*)
    (when (not (package-installed-p package))
      (package-install package))))

(provide '*required-packages*)
;;; packages.el ends here
