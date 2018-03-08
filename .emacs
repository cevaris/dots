;;; MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setq exec-path (append exec-path '("/usr/local/bin/")))

;; Cursor settings
;; (set-foreground-color "white")
;; ;; (set-background-color "black")
;; (set-cursor-color "#ffffff")
;; (setq default-frame-alist
;;   '((cursor-color . "#ffffff")))

;; Highlight text while in mark mode
(transient-mark-mode t)
;; Allways display latests file version
(auto-revert-mode t)
;; Line numbers
(global-linum-mode t)
(setq linum-format "%4d\u2502 ")
(setq require-final-newline 0)
;; enable colume mode
(setq column-number-mode 1)
;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)
;; matching parantheses
(show-paren-mode 1)
;; TAGS file is too large
(setq large-file-warning-threshold nil)
;; backup files under home directory
;; (setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq make-backup-files nil)


;; dont add to kill ring on backspace
(defun my-delete-word (arg)
    "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
    (interactive "p")
    (delete-region
     (point)
     (progn
       (forward-word arg)
       (point))))

(defun my-backward-delete-word (arg)
    "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
    (interactive "p")
    (my-delete-word (- arg)))

(defun my-delete-line ()
    "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
    (interactive)
    (delete-region
     (point)
     (progn (end-of-line 1) (point)))
    (delete-char 1))

(defun my-delete-line-backward ()
    "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
    (interactive)
    (let (p1 p2)
      (setq p1 (point))
      (beginning-of-line 1)
      (setq p2 (point))
      (delete-region p1 p2)))

;; bind them to emacs's default shortcut keys:
(global-set-key (kbd "C-S-k") 'my-delete-line-backward) ; Ctrl+Shift+k
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)


;; Projectile
;;(require 'projectile)
;;(projectile-global-mode)
;; (helm-projectile-on)
;; (setq projectile-completion-system 'helm)

;; (setq projectile-use-native-indexing t)
;; (setq projectile-indexing-method 'native)
;;(setq projectile-enable-caching t)
;; (setq projectile-use-native-indexing t)
;; (setq projectile-globally-ignored-directories
;;       (append '(".svn") projectile-globally-ignored-directories))
;;(setq projectile-globally-ignored-files (append '("*.log*" "*#*") projectile-globally-ignored-files))

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Auto mode files
(add-to-list 'auto-mode-alist '("\\BUILD\\'" . python-mode))

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


;; Auto complete
(ac-config-default)

;; hasekll-mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode);
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


;; Multiple Cursors
;; (require 'multiple-cursors)


;; Go Mode indention
;; (add-hook 'go-mode-hook
;;	  (lambda ()
;;	    (setq-default)
;;	    (setq tab-width 4)
;;	    (setq standard-indent 4)
;;	    (setq indent-tabs-mode 1)))
(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)


;; Puppet indention
(defcustom puppet-indent-level 4
  "*Indentation of Puppet statements."
    :type 'integer :group 'puppet)


;; Ensime for Scala
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

;; python Jedi - Autocomplete
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;; (setq-default python-indent 2)
;; (setq-default python-guess-indent nil)
;; (smart-tabs-advice python-indent-line-1 python-indent)
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (setq indent-tabs-mode t)
;; 	    (setq tab-width (default-value 'tab-width))))
;; (smart-tabs-advice py-indent-line py-indent-offset)
;; (smart-tabs-advice py-newline-and-indent py-indent-offset)
;; (smart-tabs-advice py-indent-region py-indent-offset)


;; code highlight
(defun annotate-code ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()")
  (highlight-lines-matching-regexp "binding.pry")
  (highlight-lines-matching-regexp "var")
  )
(add-hook 'python-mode-hook 'annotate-code)
(add-hook 'scala-mode-hook 'annotate-code)


;; shortcut table
(define-abbrev-table 'global-abbrev-table
  '(("ipdbd" "import ipdb;ipdb.set_trace()")
    ("pry" "binding.pry")
    ))


;; flymake pep8
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Enable yaml mode
(require 'yaml-mode)

;; Ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(setq ruby-deep-indent-paren nil)
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; Useless Spaces
(require 'whitespace)
;; Mode not active by default: let's activate it
(global-whitespace-mode t)
(setq whitespace-space nil)
(setq whitespace-hspace nil)
(setq whitespace-tab nil)
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

;; TAGS
;; Revist tags if changed on disk
(setq tags-revert-without-query t)
;; Auto refresh tags
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
	ad-do-it
      (error (and (buffer-modified-p)
		  (not (ding))
		  (y-or-n-p "Buffer is modified, save it? ")
		  (save-buffer))
	     (er-refresh-etags extension)
	     ad-do-it))))


;; IRC
;; (setq circe-network-options
;;       `(("Freenode"
;; 	:nick "cevaris"
;; 	:channels ("#programming)
;; 	:nickserv-password freenode-password)))

;; (defun irc ()
;;   "Connect to IRC"
;;   (interactive)
;;   (circe "Freenode")
;;   ;; (circe "Bitlbee")
;;   ;; (circe "IRCnet")
;;   ;; (circe "Snoonet")
;;   )


;; magit
(setq magit-auto-revert-mode nil)

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
	ad-do-it)
    ad-do-it))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (setq tags-table-list
		 (quote
		  ("/git/Big-Data" "/git/scala" "/git/scalaz" "/git/scalaz-stream")))
	   (setq whitespace-line-column 250)
	   (setq projectile-globally-ignored-directories
		 (append
		  (quote
		   (".git" ".ensime_cache" "*/target" "*swagger-ui*"))
		  projectile-globally-ignored-directories))
	   (setq projectile-globally-ignored-files
		 (append
		  (quote
		   ("*.xml" "*$*"))
		  projectile-globally-ignored-files)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
