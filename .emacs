;; ----------------------------------------------------------
;; Package management 

(require 'package) 

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; ----------------------------------------------------------
;; New buffer

(defun new-buffer-frame-writable ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    ;;(display-buffer buffer '(display-buffer-pop-up-frame . nil)))
    (display-buffer buffer)
    (other-window 1)
    ;;(delete-other-windows)
    ))

;; ----------------------------------------------------------
;; reload conf

(defun reload ()
  (interactive)
  (load-file "~/.emacs")
  )

;; ----------------------------------------------------------
;; Open external term

(defun open-term ()
  (interactive)
  (shell-command ( concat "open -a Terminal " default-directory))
  )

;; ----------------------------------------------------------
;; change encoding to unix

(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

;; ----------------------------------------------------------
;; Desktop SAVE

(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   1)

(desktop-save-mode 1)

;; ----------------------------------------------------------
;; Backup

(setq
 backup-directory-alist `(("." . "~/.emacs.backup"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 10
 version-control t )

;; ----------------------------------------------------------
;; ORG Mode

(require 'babel)

(setq
 org-log-done 'time
 org-todo-keywords '((sequence "OPEN" "TODO" "IN PROGRESS" "PENDING" "|" "DONE") (sequence "|" "CANCELED"  ) )
 org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (dot . t)
   ))

(add-hook 'org-mode-hook 'turn-on-font-lock)

;; ----------------------------------------------------------
;; from org mode to reveal presentation [c-c c-e R R]

(require 'ox-reveal)
(setq org-reveal-root "file:////Users/ggustin/.emacs.d/reveal.js")

;; ----------------------------------------------------------
;; Speel checking

(setq ispell-program-name "/usr/local/Cellar/aspell/0.60.6.1_1/lib/aspell-0.60/ispell")
(setenv "DICTIONARY" "fr_FR")
(setenv "LANG" "fr_FR")

;; ----------------------------------------------------------
;; Dired

(add-hook 'dired-mode-hook 'auto-revert-mode)


;; ----------------------------------------------------------
;; helm

(require 'helm)
(helm-mode 1)

;; ---------------------------------------------------------
;; Syslog mode

(require 'syslog-mode)
(add-to-list 'auto-mode-alist '("\\.log\\'" . syslog-mode))

;; ---------------------------------------------------------
;; Crontab mode

(require 'crontab-mode)
(add-to-list 'auto-mode-alist '("\\.crontab\\'" . crontab-mode))

;; ---------------------------------------------------------
;; configuration mode

(add-to-list 'auto-mode-alist '("\\.env\\'" . autoconf-mode))
(add-to-list 'auto-mode-alist '("\\.pingerrc\\'" . conf-mode))

;; ----------------------------------------------------------
;; Autocomplete

(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)

(add-to-list 'ac-modes 'org-mode)

;; ----------------------------------------------------------
;; magit

(require 'magit)

;; ----------------------------------------------------------
;; git-gutter

(require 'git-gutter)

(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

(global-git-gutter-mode +1)

;; ---------------------------------------------------------
;; Markdown

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ---------------------------------------------------------
;; Telephone line

(require 'telephone-line)
(telephone-line-mode 1)

;; ----------------------------------------------------------
;; Shotcuts

(global-set-key (kbd "C-t") 'open-term)
(global-set-key (kbd "C-c n") 'new-buffer-frame-writable)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-z") 'mc/mark-next-like-this)

(global-set-key (kbd "<C-268632083>") 'swiper-all)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-c g") 'magit-status)

(global-set-key (kbd "C-c a") 'org-agenda)

;; ----------------------------------------------------------
;; Yaml Mode

(require 'yaml-mode)

;; ----------------------------------------------------------
;; Look

(set-cursor-color "#ff0000")
(add-to-list 'default-frame-alist '(font . "Source Code Pro for Powerline-12"))

;; ----------------------------------------------------------
;; Add password generator

(require 'password-generator)

;; ----------------------------------------------------------
;; configuration

(defalias 'yes-or-no-p 'y-or-n-p)

(setq  
 compilation-always-kill t             
 compilation-scroll-output 'first-error
 confirm-kill-processes nil            
 create-lockfiles nil                  
 enable-recursive-minibuffers t        
 inhibit-startup-screen t              
 kill-whole-line t                     
 mac-drawing-use-gcd t                 
 mac-mouse-wheel-smooth-scroll nil     
 mark-even-if-inactive nil             
 require-final-newline t               
 ring-bell-function 'ignore            
 save-interprogram-paste-before-kill t 
 sentence-end-double-space nil
 use-dialog-box nil           
 user-full-name "Gustin Gaël" 
 auto-revert-verbose nil   
 auto-save-default nil
 column-number-mode t
 global-auto-revert-mode 1
 inhibit-startup-message t
 mac-command-key-is-meta t
 mac-command-modifier 'meta
 mac-option-modifier nil
 message-kill-buffer-on-exit t
 org-agenda-files (file-expand-wildcards "~/taktik/todo.org")
 save-interprogram-paste-before-kill t
 set-keyboard-coding-system nil
 tramp-completion-reread-directory-timeout nil
 tramp-default-method "ssh"
 truncate-lines t
 visible-bell t
 telephone-line-height 18
 custom-file "~/.emacs.d/custom.el"
 frame-title-format (list '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
 )

(load custom-file)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)  
(show-paren-mode t)      
(delete-selection-mode t)
(save-place-mode)


(server-start)
