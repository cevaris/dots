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
(setq require-final-newline 0)

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
(require 'whitespace)
;; Mode not active by default: let's activate it
(global-whitespace-mode t)
(setq whitespace-space 0)
(setq whitespace-display-mappings
      '((space-mark ?\ [] []); space
	(newline-mark ?\n [?\xB7 ?\n] [?. ?\n]); space
	))


;; Window change
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)


;; Copy from Emacs
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


(define-abbrev-table 'global-abbrev-table '(
					    ("ipdbd" "import ipdb;ipdb.set_trace()")
					    ))
;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)


;; ispell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq ispell-program-name "/usr/local/Cellar/ispell/3.3.02/bin/ispell")



