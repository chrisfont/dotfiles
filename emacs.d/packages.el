;; Packages.el
;; Emacs Configuration file

;; This file provides settings for ELPA packages
;; for my Emacs configuration, including whether or not
;; packages are required.

(require 'cl)

;; ELPA Repos
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa"     . "http://melpa.milkbox.net/packages/")))



;; Finally, finish package init and return
(package-initialize)

;; Required Package Lists
(defvar *required-packages*
  '(clojure-mode flycheck flycheck-d-unittest flycheck-color-mode-line flycheck-tip
		 flycheck-ledger graphene haskell-mode lua-mode magit markdown-mode
		 monokai-theme org pianobar python-mode pyflakes slime zeal-at-point))


;; Package auto-installation code -- Thanks bbatsov
(defun needed-packages-installed-p ()
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

