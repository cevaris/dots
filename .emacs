;; MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; Projectile
(projectile-global-mode +1)

;; Highlight text while in mark mode
(transient-mark-mode t)

;; Line numbers
;; (global-linum-mode t)
;; (setq linum-format "%d "k)


;; auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(ac-config-default)


;; hasekll-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Multiple Cursors
(require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Go Mode indention
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-default)
	    (setq tab-width 4)
	    (setq standard-indent 4)
	    (setq indent-tabs-mode 1)))

;; Puppet indention
(defcustom puppet-indent-level 4
  "*Indentation of Puppet statements."
    :type 'integer :group 'puppet)

;; Ensime for Scala
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;; Python Jedi - Autocomplete
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq-default python-indent 2)
(setq-default python-guess-indent nil)


;; Flymake pep8
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Enable yaml mode
(require 'yaml-mode)


;; enable colume mode
(setq column-number-mode 1)


;; Useless Spaces
(setq-default show-trailing-whitespace t)


;; Window change
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)


