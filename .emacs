;; MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; Highlight text while in mark mode
(transient-mark-mode t)

;; Line numbers
;; (global-linum-mode t)
;; (setq linum-format "%d "k)


;; auto-complete
(add-to-list 'load-path "/Users/adamc/.emacs.d/auto-complete")
(require 'auto-complete-config)
(ac-config-default)

;; ctags
;;(require 'ctags)

;; hasekll-mode
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode) 

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Go Mode indention
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-default)
	    (setq tab-width 4)
	    (setq standard-indent 4)
	    (setq indent-tabs-mode 1)))
