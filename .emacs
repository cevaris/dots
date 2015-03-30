;; MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; Projectile
(require 'projectile)
(projectile-global-mode)
(helm-projectile-on)
;; (setq projectile-completion-system 'helm)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-globally-ignored-files "")
;; (setq projectile-globally-ignored-directories "target")

(setq projectile-globally-ignored-directories
      (append projectile-globally-ignored-directories '(".git"
							"build"
							"target"
							)))


;; etags-select
(require 'etags-select)
(setq etags-select-mode 1)

(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
(define-key etags-select-mode-map (kbd "C-g")   'etags-select-quit)
(define-key etags-select-mode-map (kbd "C-x o") 'etags-select-quit)
(define-key etags-select-mode-map (kbd "C-x O") 'etags-select-quit)
(define-key etags-select-mode-map (kbd "C-p")   'etags-select-previous-tag)
(define-key etags-select-mode-map (kbd "C-n")   'etags-select-next-tag)
(define-key etags-select-mode-map (kbd "RET") 'etags-select-goto-tag)
(define-key etags-select-mode-map (kbd "M-RET") 'etags-select-goto-tag-other-window)

;; Highlight text while in mark mode
(transient-mark-mode t)

;; Line numbers
;; (global-linum-mode t)
;; (setq linum-format "%d "k)
(setq require-final-newline 0)


;; Auto complete
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
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;; Python Jedi - Autocomplete
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq-default python-indent 2)
(setq-default python-guess-indent nil)


;; ipdb highlight
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)


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
	(newline-mark ?\n [?\u00AC ?\n]); newline
	)
)

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


;; shortcut table
(define-abbrev-table 'global-abbrev-table '(
					    ("ipdbd" "import ipdb;ipdb.set_trace()")
					    ))
;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)


;; smooth scrolling
;; Top/Botom page scrolling
(setq auto-save-interval 500)
(setq scroll-conservatively 10000)
:; Scoll in place window scrolling
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))
(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))
(global-set-key "\M-n" 'scroll-up-in-place)
(global-set-key "\M-p" 'scroll-down-in-place)


;; matching parantheses
(show-paren-mode 1)

;; TAGS file is too large
(setq large-file-warning-threshold nil)

;; ispell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq ispell-program-name "/usr/local/Cellar/ispell/3.3.02/bin/ispell")


