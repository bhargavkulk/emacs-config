;;; -*- lexical-binding: t -*-
;; Author: Bhargav Kulkarni

;; Set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

;; Start emacs at ~/Projects
(defun startup-mode ()
  "Open Dired in ~/Projects/ if no file is provided at startup."
  (when (and (eq (length command-line-args) 1)       ; Only Emacs itself in args
             (not (member "-nw" command-line-args))) ; Not in terminal mode
    (dired "~/Projects/")))

;; dired warning in Mac
(when (string= system-type "darwin")
(let ((gls-path (executable-find "gls")))
  (if gls-path
      (setq dired-use-ls-dired t
            insert-directory-program gls-path
            dired-listing-switches "-aBhl --group-directories-first")
    (setq dired-use-ls-dired nil))))

(defun initfile () (interactive)
       (find-file (concat user-emacs-directory "/init.el")))

(defun reconfigure () (interactive)
       (load-file (concat user-emacs-directory "/init.el")))

(setopt inhibit-startup-screen             t            ; Disable the startup screen
	 	inhibit-startup-message            t            ; Suppress the startup message in the *Messages* buffer
		inhibit-startup-echo-area-message  "bhargavkk") ; Suppress the startup message in minibuffer

(use-package emacs
  :init
  (load-theme 'ef-night)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (setopt visible-bell                          t ; Don't want Emacs making noise
          display-time-default-load-average     nil ; Don't show load time in the mode line
          sentence-end-double-space             nil ; Don't require double spaces after sentence
          make-backup-file-name-function        'backup-file-name ; Clean backup files
          mouse-wheel-tilt-scroll               t ; Enable horizontal scrollin
          mouse-wheel-flip-direction            t ; Reverse mouse wheel scroll
          column-number-mode                    t ; Show the current column number
          size-indication-mode                  t ; Show the size of the buffer in the modeline
          require-final-newline                 t ; Always ensure files end with a newline
          tab-always-indent                     'complete
          indicate-buffer-boundaries            'left ; Show buffer boundaries on the left side
          use-short-answers                     t ; Use 'y'/'n' instead of 'yes'/'no' for yes/no prompts
          save-interprogram-paste-before-kill   t ; Save text to the clipboard before killing it
          history-length                        2 ; Limit the saved minibuffer history to the last 25 entries
	      read-buffer-completion-ignore-case    t ; Ignore case when completing buffer names
          read-file-name-completion-ignore-case t ; Ignore case when completing file names
          find-file-suppress-same-file-warnings t ; Just redirect to the existing buffer
          backward-delete-char-untabify-method  'nil) ; Make backspace remove entire indents

  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq-default major-mode                     'text-mode
                fill-column                    80
                tab-width                      4
                indent-tabs-mode               nil
                cursor-in-non-selected-windows t
                bidi-display-reordering        nil
                create-lockfiles               nil
                word-wrap                      t
                cursor-type                    'bar)

  (when (member "CommitMonoOneAndHalf" (font-family-list))
    (set-face-attribute 'default nil :font "CommitMonoOneAndHalf" :height 200)
    (set-face-attribute 'fixed-pitch nil :font "CommitMonoOneAndHalf" :height 200))

  (when (member "iA Writer Duo S" (font-family-list))
    (set-face-attribute 'variable-pitch nil
                        :font "iA Writer Duo S" :height 200 :weight 'thin))

  :config
  (tooltip-mode -1)      ; Remove tooltips
  (blink-cursor-mode -1) ; Stop cursor from blinking

  :hook ((before-save . delete-trailing-whitespace)) ; Delete trailing whitespaces

  :bind (("<escape>" . keyboard-escape-quit) ; Quit on escape
	     ("C-x C-z" . nil)                   ; Unset suspend-frame
         ("C-<wheel-up>" . nil)              ; Unset text scale up
         ("C-<wheel-down>" . nil)            ; Unset text scale down
	     :map minibuffer-mode-map
	     ("TAB" . minibuffer-complete)))     ; Minibuffer completion on tab

;; Automatch brackets
(use-package electric-pair
  :ensure nil
  :hook   prog-mode)

;; Display line numbers
(use-package display-line-numbers
  :ensure nil
  :hook   prog-mode
  :init   (setopt display-line-numbers-width 3))

;; Nice line wrapping, instead of truncation
(use-package visual-line
  :ensure nil
  :hook   text-mode)

;; Reread files from disk if changed
(use-package autorevert
  :ensure nil
  :init   (setopt auto-revert-avoid-polling t
		          auto-revert-interval 5
		          auto-revert-verbose nil
		          auto-revert-check-vc-info t
		          global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :config (savehist-mode))

;; Pixel perfect scroll
(use-package pixel-scroll
  :ensure nil
  :config (pixel-scroll-precision-mode))

;; Fringe settings
(use-package fringe
  :ensure nil
  :config (set-fringe-mode 10))

;; Yanking should replace selection
(use-package delsel
  :ensure nil
  :config (delete-selection-mode t))

;; Improve performance for files with long lines
(use-package so-long
  :ensure nil
  :config (global-so-long-mode t))

;; Remember recently open files
(use-package recentf
  :ensure nil
  :init
  (setopt recentf-max-saved-items 1000
          recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (recentf-mode t))

;; Remember cursor positions in files
(use-package saveplace
  :ensure nil
  :config (save-place-mode t))

;; eshell config
(use-package eshell
  :defer t
  :config
   (add-hook 'eshell-preoutput-filter-functions
             'ansi-color-filter-apply))

;; Moving between windows. Lends itself well to =meow=.
;; Using meow just have to press "<space> <arrow-key>".
(use-package windmove
  :ensure nil
  :init   (windmove-default-keybindings 'control))

;; Neatly distinguish between two files of the same name with different paths.
(use-package uniquify
  :ensure nil
  :init   (setopt uniquify-buffer-name-style 'forward))

;; Some frame enhancements
(use-package frame
  :ensure nil
  :hook   (window-setup . window-divider-mode)
  :init   (setopt window-divider-default-places       t   ; Enable window dividers in Emacs
                  window-divider-default-bottom-width 1   ; Set the width of the divider at the bottom to 1
                  window-divider-default-right-width  1)) ; Set the width of the divider on the right side to 1

;; Minibuffer enhancements
(use-package minibuffer
  :ensure nil
  :init
  (setopt completion-cycle-threshold   1                           ; TAB cycles candidates
		  completions-detailed         t                           ; Show annotations
		  completion-styles            '(basic initials substring) ; Styles to match input to candidates
		  completion-auto-help         'always                     ; Always open completion
		  completions-max-height       20                          ; Set arbitrary max height
		  completions-format           'one-column                 ; One-column display
		  completions-group            t                           ; Group completions
		  completion-auto-select       'second-tab))               ; Eagerly auto-select on second TAB

(use-package mixed-pitch
  :ensure t)

(use-package org
  :defer t
  :hook mixed-pitch-mode)

;; which-key is a minor mode for Emacs that displays the key bindings
;; following your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; exec-path-from-shell ensures environment variables inside Emacs look the
;; same as in the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :if     (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;; Vertico provides a performant and minimalistic vertical completion UI based
;; on the default completion system.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after  vertico)

;; `marginalia' adds annotations to the minibuffer completions.
(use-package marginalia
  :init
  (marginalia-mode))

;; `consult' provides search and navigation commands based on the Emacs completion
;; function completing-read. Completion allows you to quickly select an item
;; from a list of candidates.
(use-package consult
  :ensure t
  :bind   (("C-x b" . consult-buffer)   ; orig. switch-to-buffer
           ("C-x C-b" . consult-buffer)
           ("M-y"   . consult-yank-pop) ; orig. yank-pop
           ("C-s" . consult-line)       ; Alternative: rebind C-s to use
           ("M-s o" . consult-outline)) ; outline
  :config
  (setq consult-narrow-key "<"))

;; `orderless' provides an orderless completion style that divides the pattern
;; into space-separated components, and matches candidates that match all of
;; the components in any order.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; `corfu' gives nice popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Cape provides Completion At Point Extensions which can be used in combination
;; with Corfu, Company or the default completion UI.
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; `racket-mode' for editing racket files
(use-package racket-mode
  :defer t
  :ensure t)

(use-package eglot
  ;; no :ensure t here because it's built-in
  :defer t
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (setq eglot-highlight-symbol nil)
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (setq-default eglot-workspace-configuration
		  '((haskell
		     (plugin
		      (stan
		       (globalOn . :json-false))))))
  (add-to-list 'eglot-server-programs
		 '(racket-mode . ("racket" "-l" "racket-langserver")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))'

(add-hook 'racket-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'haskell-mode 'eglot-ensure)
(add-hook 'c-mode 'eglot-ensure)
(add-hook 'c++-mode 'eglot-ensure)

(use-package hydra
  :ensure t)

(defhydra hydra-command (:columns 4)
  "Command map"
  ("w" save-file "Save File" :color blue)
  ("f" find-file "Find File" :color blue)
  ("r" recentf "Recent Files" :color blue)
  ("t" project-eshell "Open Terminal" :color blue))

;; Nice Modal Editing
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . consult-line)
   '(":" . hydra-command/body)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package grid
  :init
  (unless (package-installed-p 'grid)
    (package-vc-install
     '(grid
       :vc-backend Git
       :url "https://github.com/ichernyshovvv/grid.el"
       :branch "master"))))

(defface enlight-violet
  '((t (:foreground "#af00d7" :width expanded)))
  "Violet face for dashboard.")

(defvar enlight-emacs
  (propertize
   "███████╗███╗   ███╗ █████╗  ██████╗███████╗
██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
█████╗  ██╔████╔██║███████║██║     ███████╗
██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"
   'face 'enlight-violet))

(use-package enlight
  :ensure t
  :init
  (setopt initial-buffer-choice #'enlight)
  :custom
  (enlight-content
   (concat
    (grid-get-box `( :align center :content ,enlight-emacs  :width 80))
    "\n\n"
    (grid-get-box
     `(:align center
       :width 80
       :content
       ,(enlight-menu
         '(("Files"
	        ("Recent" (consult-recent-file) "r"))
           ("Other"
	        ("Projects" project-switch-project "p")))))))))

(use-package olivetti
  :ensure t)

(setq-default olivetti-body-width 100)

(use-package auto-olivetti
  :config
  (setopt auto-olivetti-enabled-modes '(text-mode prog-mode))
  (auto-olivetti-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package nerd-icons
  :ensure t)

(use-package magit
  :ensure t)

(set-face-attribute 'mode-line nil
                    :box nil)  ;; Removes the border (box) from the mode line

(custom-set-faces
  '(modus-themes-lang-error ((t (:underline (:color "#ff4a6f" :style line)))))
  '(modus-themes-lang-warning ((t (:underline (:color "#d0de00" :style line)))))
  '(modus-themes-lang-note ((t (:underline (:color "#5f6fff" :style line))))))

(defun gitstatus ()
  (when vc-mode
    (when (buffer-file-name)
      (pcase (vc-state (buffer-file-name))
        ('up-to-date "✓")
        ('edited "*")
        ('needs-update "∴")
        ('added "+")
        ('removed "-")
        ('conflict "!")
        ('ignored "~")
        (_ "?")))))

(setq-default mode-line-format
              (list
               ;; Display modified buffer icon (flame if modified)
               '(:eval (propertize " λ"
                                  'face (if (buffer-modified-p)
                                            '(:foreground "red")
                                          'mode-line-buffer-id)))
               " "
               "%b"
               " "
               ;; Display major mode
               ;;'(:propertize mode-name face mode-line-buffer-id)
               mode-name
               ;; Display Git branch if inside a Git repo
               '(:eval (if (and (boundp 'vc-mode) vc-mode)
                          (let ((branch-name (substring vc-mode 5)))  ;; Extract branch name
                            (propertize (concat " [" branch-name "|" (gitstatus) "]") 'face 'mode-line-buffer-id))
                         ""))
               " |"
	           '(:propertize "%4l:%c" face mode-line-buffer-id)
               " | "
               '(:eval (propertize (format-time-string "%a %d, %Y %I:%M %p") 'face 'mode-line-buffer-id
                                   ))))
