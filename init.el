;;; Bhargav Kulkarni's init.el

;;; Constants
(defconst macos?
  (eq system-type 'darwin))

;;; Functions
(defun bk/split-window-sensibly (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal
  split, i.e. windows tiled side-by-side."
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
        (and
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-width-threshold 0))
           (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))

(defun bk/scroll-up-center ()
  "Scroll up and recenter cursor vertically."
  (interactive)
  (scroll-up-command)
  (recenter))

(defun bk/scroll-down-center ()
  "Scroll down and recenter cursor vertically."
  (interactive)
  (scroll-down-command)
  (recenter))

(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

(defun reload-init-file ()
  "Reload init file."
  (interactive)
  (load user-init-file))

(defun open-init-file ()
  "Open init file."
  (interactive)
  (find-file user-init-file))

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (prog-mode))

;;; Customizing Stuff
;;;; User Info
(setopt user-full-name "Bhargav"
	    user-mail-address "bhargavkishork@gmail.com")

;;;; Custom-File
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(and (file-readable-p custom-file) (load custom-file))

;;;; UTF-8 as default
(when (fboundp 'set-charser-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)

;;;; Saner Defaults
(setq-default major-mode       'text-mode
	          fill-column      80
	          tab-width        4
	          indent-tabs-mode nil
	          create-lockfiles nil
	          word-wrap        t
	          truncate-lines   t
	          cursor-type      'bar)

(setopt backup-directory-alist '(("." . "~/.config/emacs/backups"))
        make-backup-files t    ;; ensure backups are enabled
        backup-by-copying t    ;; don't clobber symlinks
        version-control t      ;; use versioned backups
        delete-old-versions t  ;; delete excess backup versions
        kept-new-versions 6
        kept-old-versions 2)

(setopt confirm-kill-emacs #'y-or-n-p
        use-short-answers t

        tab-always-indent 'complete
        completion-cycle-threshold 1
        completions-detailed t
        completion-styles '(basic initials substring)
        completion-auto-help 'always
        completions-max-height 20
        completions-format 'one-column
        completions-group t
        completion-auto-select 'second-tab

        split-height-threshold 4
        split-width-threshold 80
        split-window-preferred-function 'bk/split-window-sensibly

        delete-by-moving-to-trash t
        make-backup-file nil
        inhibit-compacting-font-caches t

        inhibit-startup-screen t
        inhibit-startup-message t

        auto-save-default t
        auto-save-include-big-deletions t
        auto-save-list-file=prefix (expand-file-name "autosaves/" user-emacs-directory)
        auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                                   ;; Prefix tramp autosaves to prevent conflicts with local ones
                                                   (concat auto-save-list-file-prefix "tramp-\\2") t)
                                             (list ".*" auto-save-list-file-prefix t))

        uniquify-buffer-name-style 'forward

        find-file-suppress-same-file-warnings t

        truncate-string-ellipsis "..."

        shell-command-prompt-show-cwd t

        read-extended-command-predicate #'command-completion-default-include-p

        require-final-newline t
        sentence-end-double-space nil

        confirm-nonexistent-file-or-buffer nil

        find-file-visit-truename t
        vc-follow-symlinks t

        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t

        visible-bell t

        idle-update-delay 1.0
        highlight-nonselected-windows nil
        fast-but-imprecise-scrolling t
        redisplay-skip-fontification-on-input t
        auto-hscroll-mode t
        hscroll-step 0
        hscroll-margin 2

        auto-window-vscroll nil

        frame-inhibit-implied-resize t

        frame-resize-pixelwise t
        window-resize-pixelwise t
        bidi-inhibit-bpa t
        bidi-paragraph-direction 'left-to-right
        bidi-display-reordering 'left-to-right

        long-line-threshold 1000
        large-hscroll-threshold 1000
        syntax-wholeline-max 1000

        save-interprogram-paste-before-kill t

        kill-whole-line t
        line-move-visual t
        track-eol t

        text-mode-ispell-word-completion nil

        column-number-mode t
        line-number-mode t

        frame-title-format '("Emacs - %b")
        icon-title-format frame-title-format

        use-file-dialog nil
        use-dialog-box nil

        window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1

        indicate-buffer-boundaries 'left
        display-line-numbers-width-start t

        history-length 25

        backward-delete-char-untabify-method nil
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

(when macos?
  (setopt ns-use-thin-smoothing t
          ns-pop-up-frames nil))

;;;; Hooks
(add-hook 'window-setup-hook #'window-divider-mode)
(add-hook 'text-mode-hook
          #'(lambda ()
              (remove-hook 'completion-at-point-functions 'ispell-completion-at-point t)))
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;;; Minor Mode Config
(tooltip-mode 0)
(blink-cursor-mode 0)
(savehist-mode)
(with-eval-after-load 'vertico
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))
(add-hook 'savehist-save-hook
          #'(lambda ()
              (setq kill-ring (mapcar #'substring-no-properties
                                      (cl-remove-if-not #'stringp kill-ring))
                    search-ring (mapcar #'substring-no-properties search-ring)
                    regexp-search-ring (mapcar #'substring-no-properties regexp-search-ring)
                    register-alist (cl-loop for (reg . item) in register-alist
                                            if (stringp item)
                                            collect (cons reg (substring-no-properties item))
                                            else collect (cons reg item)))))

(set-fringe-mode 10)
(delete-selection-mode t)
(global-so-long-mode t)
(save-place-mode t)

(global-auto-revert-mode)
(setopt auto-revert-avoid-polling t
        auto-revert-check-vc-info t
        auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)

(recentf-mode t)
(setopt recentf-max-saved-items 25
        recentf-exclude
        (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
              "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
              "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
              "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
              (lambda (file) (file-in-directory-p file package-user-dir))))
(push (expand-file-name recentf-save-file) recentf-exclude)
(add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
(add-to-list 'recentf-filename-handlers #'substring-no-properties)

(electric-pair-mode t)
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (or (char-equal c ?\') (char-equal c ?\")) t (electric-pair-default-inhibit c))))

(which-key-mode 1)
(setq which-key-max-description-length 30
      which-key-lighter nil
      which-key-show-remaining-keys t)

(defun bk/select-window (window &rest _)
  "Select WINDOW for display-buffer-alist"
  (select-window window))
(setq display-buffer-alist
      '(((or . ((derived-mode . occur-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (body-function . bk/select-window)
         (dedicated . t)
         (preserve-size . (t . t)))))



;;; Global KeyBindings
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
;; (global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-<wheel-up>") nil)
(global-set-key (kbd "C-<wheel-down>") nil)
(global-set-key (kbd "C-x C-r") #'recentf)

;;; Fonts
(defvar bk/font-size (if macos? 15 20))
(defvar bk/font-monospace "Iosevka Clear")
(defvar bk/font-duospace "Iosevka Clear Type")
(defvar bk/font-symbol "Julia Mono Regular")

(defun bk/setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    (set-face-attribute 'fixed-pitch nil
                        :font (font-spec :family bk/font-monospace
                                         :size bk/font-size))
    (set-face-attribute 'default nil
                        :font (font-spec :family bk/font-monospace
                                         :size bk/font-size))
    (set-face-attribute 'variable-pitch nil
                        :font (font-spec :family bk/font-duospace
                                         :size bk/font-size))
    (set-fontset-font t 'symbol (font-spec :family bk/font-symbol) nil 'prepend)))

(bk/setup-fonts)
(add-hook 'window-setup-hook #'bk/setup-fonts)
(add-hook 'server-after-make-frame-hook #'bk/setup-fonts)

;;; Elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; isearch stuff
(use-package isearch
  :ensure nil
  :config
  (defun bk/occur-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (occur query)))

  (defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
    (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
        (progn
          (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
          (deactivate-mark)
          ad-do-it
          (if (not forward)
              (isearch-repeat-backward)
            (goto-char (mark))
            (isearch-repeat-forward)))
      ad-do-it))
  :bind
  (:map isearch-mode-map
        ("C-." . isearch-forward-symbol-at-point)
        ("C-o" . bk/occur-from-isearch)))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; UI Stuff
;;;; Theme
(use-package solarized-theme
  :ensure (:repo "https://github.com/bhargavkulk/solarized-theme")
  :init
  (setopt solarized-highlight-numbers nil)
  :config
  (load-theme 'solarized-selenized-black t))

;;;; Spacious Padding
(use-package spacious-padding
  :ensure t
  :init
  (setopt spacious-padding-widths '(:internal-border-width 5
                                                           :header-line-width 4
                                                           :mode-line-width 4
                                                           :right-divider-width 1
                                                           :fringe-width 8)
          spacious-padding-subtle-mode-line t)
  :config
  (spacious-padding-mode 1))

;;;; Breadcrumbs | Header Line
(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode))

;;;; UltraScroll | Pixel Precise Scrolling
(use-package ultra-scroll
  :ensure (:repo "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;;;; Mixed Pitch
(use-package mixed-pitch
  :after solarized-theme
  :hook ((markdown-mode org-mode adoc-mode) . (lambda () (mixed-pitch-mode +1))))

;;;; Exec Path From Shell | MACOS shenanigans
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;;;; Helpful | better describe
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point)))

;;; Completions
;;;; Orderless
(use-package orderless
  :config
  (setopt completion-styles '(orderless basic)
          completion-category-overrides '((file (styles basic partial-completion)))))

;;;; Corfu | Completion IO
(use-package corfu
  :config
  (setopt corfu-cycle t)
  :init
  (global-corfu-mode))

(use-package corfu-history
  :after corfu
  :ensure nil
  :config
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
  (corfu-history-mode t))

(use-package corfu-echo
  :after corfu
  :ensure nil
  :init
  (corfu-echo-mode t)
  :config
  (setopt corfu-popupinfo-delay '(1.0 . 0.5)))

;;;; Vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :config
  (setopt vertico-buffer-display-action '(display-buffer-in-direction
                                          (direction . right)
                                          (window-width . 0.3))))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;;;; Consult
(use-package consult
  :after vertico
  :ensure t
  :bind (([remap switch-to-buffer] . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ([remap yank-pop]   . consult-yank-pop))
  :config
  (setopt vertico-multiform-commands
          `((consult-imenu buffer ,(lambda (_) (text-scale-set -1)))
            (consult-outline buffer ,(lambda (_) (text-scale-set -1))))
          consult-narrow-key "<"))

;;; Hydra
(use-package hydra)

;;; Avy
(use-package avy
  :ensure t
  :after hydra
  :bind (:map isearch-mode-map
              ("C-j" . avy-isearch))
  :config
  (setopt avy-all-windows nil
          avy-all-windows-alt t
          avy-background t
          avy-style 'pre)
  :init
  (defhydra hydra-avy (:exit t :hint nil)
    ("y" avy-copy-line "Yank" :column "Line")
    ("m" avy-move-line "Move")
    ("k" avy-kill-whole-line "Kill")
    ("Y" avy-copy-region "Yank" :column "Region")
    ("M" avy-move-region "Move")
    ("K" avy-kill-region "Kill")
    ("c" avy-goto-char-timer "Timed Char" :column "Goto")
    ("C" avy-goto-char "Char")))

;;; RipGrep
(use-package rg
  :ensure t
  :init
  (defhydra hydra-rg (:exit t :hint nil)
    ("f" rg-dwim-current-file "Current File" :column "Ripgrep")
    ("." rg-dwim-current-dir "Current Folder")
    ("p" rg-dwim-current-project "Current Project")))

(use-package org
  :ensure nil
  :config
  (setq org-startup-with-inline-images t
        org-startup-folded 'showall
        org-default-notes-file "~/Notes/append.org")

  (setq org-capture-templates
        '(("a" "Append Note"
           entry
           (file "~/Notes/append.org")
           "* %U\n%?\n"
           :empty-lines 1)))

  (global-set-key (kbd "C-c c") #'org-capture))

;;; Project
(use-package project
  :ensure nil
  :after (hydra magit rg)
  :config
  (global-set-key (kbd "C-x p") 'hydra-project/body)

  (defun project-rg (query)
    "Run ripgrep in the current project's root directory with QUERY."
    (interactive "sRipgrep search: ")
    (let ((default-directory (or (project-root (project-current t))
                                 default-directory)))
      (rg query "*" default-directory)))

  (global-set-key (kbd "C-c p r") #'project-rg)

  (defhydra hydra-project (:color blue)
    ("p" project-switch-project "Switch Project" :column "Project")
    ("f" project-find-file "Open File")
    ("d" project-dired "Open Dired")
    ("m" magit-project-status "Git")
    ("e" project-eshell "Open EShell" :column "Shell")
    ("c" project-compile "Run Command")
    ("g" consult-ripgrep "Consult Ripgrep" :column "Search")
    ("r" project-rg "rg")))

;;; Magit
(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer
           (cond ((and (derived-mode-p 'magit-mode)
                       (eq (with-current-buffer buffer major-mode)
                           'magit-status-mode))
                  nil)
                 ((memq (with-current-buffer buffer major-mode)
                        '(magit-process-mode
                          magit-revision-mode
                          magit-diff-mode
                          magit-stash-mode))
                  nil)
                 (t
                  '(display-buffer-same-window)))))))

(use-package diff-hl
  :ensure t
  :after magit
  :config
  (add-hook 'prog-mode 'diff-hl-mode))

(use-package hl-todo
  :ensure t
  :after magit
  :config
  (add-hook 'prog-mode 'hl-todo-mode))

(use-package expreg
  :ensure t)

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/howm"))
  (setq denote-known-keywords (list "life" "bhargav" "emacs" "philosophy"))
  (denote-rename-buffer-mode 1))

(use-package howm
  :ensure t
  :init
  (require 'howm-org)
  :config
  (defun bk/howm-basename-chop (str)
    "Advice for `howm-view-item-basename'.
Takes a file's basename, STR, and returns only the portion before \"--\",
with timestamps like \"20250711T111213\" converted to \"2025-07-11-111213\"."
    (let ((dashes-pos (string-match "--" str)))
      (cond
       (dashes-pos
        (let ((ts (substring str 0 dashes-pos)))
          (format "%s-%s-%s-%s"
                  (substring ts 0 4)   ;; YYYY
                  (substring ts 4 6)   ;; MM
                  (substring ts 6 8)   ;; DD
                  (substring ts 9 15)  ;; HHMMSS (skip 'T')
                  )))
       (t str))))

  (defun bk/howm-cut-title (str)
    "Remove `#+title:` plus any following whitespace from STR if it starts with it."
    (if (and str
             (string-prefix-p "#+title:" (string-trim-left str)))
        (string-trim-left
         (string-remove-prefix "#+title:" (string-trim-left str)))
      str))

  (advice-add 'howm-view-item-basename
              :filter-return 'bk/howm-basename-chop)
  (advice-add 'howm-view-item-summary
              :filter-return 'bk/howm-cut-title)

  (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org"
        howm-view-title-header "#+title:"
        howm-dtime-format "[%Y-%m-%d %a %H:%M]"
        howm-menu-file-extension ".org"))

(defun bk/meow-open-line ()
  (interactive)
  (open-line 1)
  (meow-insert))

(defun bk/meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (if (not (region-active-p))
        (when (and (not (use-region-p))
                   (< (point) (point-max)))
          (forward-char 1))
      (meow--direction-forward)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun bk/meow-join ()
  "Joins line with line below it."
  (interactive)
  (meow-join -1)
  (meow-kill))

(use-package surround
  :ensure t)

;;; Meow
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

  ;; Meta
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
   '("q" . meow-quit)
   '("Q" . meow-cancel-selection)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("o" . bk/meow-append)
   '("O" . meow-open-below)
   '(":" . execute-extended-command)
   '("<escape>" . ignore))

  ;; Movement
  (meow-normal-define-key
   '("-" . meow-reverse)
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
   '("a" . meow-back-word)
   '("A" . meow-back-symbol)
   '("s" . meow-next-word)
   '("S" . meow-next-symbol)
   '("g" . avy-goto-word-1)
   '("G" . avy-goto-line)
   '("f" . isearch-forward)
   '("F" . isearch-backward)
   '("[" . bk/scroll-up-center)
   '("]" . bk/scroll-down-center)
   '("{" . beginning-of-buffer)
   '("}" . end-of-buffer)
   '("$" . back-to-indentation)
   '("^" . end-of-line))

  ;; Selection
  (meow-normal-define-key
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("e" . meow-line)
   '("z" . meow-join)
   '("'" . expreg-expand)
   '("\"" . expreg-contract))

  ;; Edits
  (meow-normal-define-key
   '("x" . bk/meow-join)
   '("X" . newline-and-indent))

  ;; Selection Verbs
  (meow-normal-define-key
   '("d" . meow-kill)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("c" . meow-change)
   '("C" . meow-replace)
   '(";" . comment-dwim)
   '("\\" . fill-paragraph))

  ;; Hydras
  (meow-normal-define-key
   '("/a" . hydra-avy/body)
   '("/p" . hydra-project/body)
   '("/e" . hydra-eglot/body)
   '("/r" . hydra-rg/body)))

(use-package meow
  :ensure t
  :config
  (setopt meow-keypad-leader-dispatch "C-x"
          meow-keypad-ctrl-meta-prefix ?\\)
  (setq-default meow-replace-state-name-list
                '((normal . "NOR")
                  (motion . "MTN")
                  (keypad . "KPD")
                  (insert . "INS")
                  (beacon . "BCN")))
  (meow-setup)
  (meow-global-mode 1))

;;; Vundo | better undo
(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  :init
  (add-hook 'vundo-mode-hook (lambda () (setq-local cursor-type nil))))

(use-package perfect-margin
  :config
  (setopt perfect-margin-visible-width 110))

(use-package popper
  :custom
  (popper-group-function #'popper-group-by-directory)
  (popper-echo-dispatch-actions t)
  :bind (:map popper-mode-map
              ("C-x `" . popper-toggle)
              ("C-M-`" . popper-cycle)
              ("C-M-<tab>" . popper-toggle-type))
  :hook ((emacs-startup . popper-echo-mode))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*Completions\\*$"
          "\\*Warnings\\*$"
          "\\*Async Shell Command\\*$"
          "\\*Apropos\\*$"
          "\\*Backtrace\\*$"
          "\\*Calendar\\*$"
          "\\*Finder\\*$"
          "\\*Kill Ring\\*$"
          "\\*Go-Translate\\*$"
          "\\*Embark \\(Collect\\|Live\\):.*\\*$"

          bookmark-bmenu-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode


          "^\\*Process List\\*$" process-menu-mode list-environment-mode cargo-process-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\**"
          "\\*diff-hl\\**"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$"
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode
          "\\*rustfmt\\*$"))
  (setq popper-window-height 0.33)
  :config
  (setopt popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis)))
  (popper-mode 1)
  (popper-echo-mode 1))

(setq major-mode-remap-alist '((python-mode . python-ts-mode)))

;;; Programming Stuff
;;;; Apheleia | Linter
(use-package apheleia
  :ensure t
  :defer t
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'rust-mode apheleia-formatters)
        '("rustfmt" "--edition" "2024" "--quiet" "--emit" "stdout")))

;;;; Eglot
(use-package eglot
  :ensure nil
  :defer t
  :after hydra
  :bind (("C-x e" . hydra-eglot/body))
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :init
  (defhydra hydra-eglot (:color blue)
    ("s" eglot "Start LSP")
    ("a" eglot-code-actions "Code Actions")
    ("r" eglot-rename "Rename")
    ("k" eglot-shutdown "Shutdown LSP"))
  :config
  (setq eglot-highlight-symbol nil)
  (fset #'jsonrpc--log-event #'ignore))

;;;; Python Stuff
(defun venv-activate ()
  "Activate Python environment managed by uv based on current project directory.
  Looks for .venv directory in project root and activates the Python interpreter."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (venv-path (expand-file-name ".venv" project-root))
         (python-path (expand-file-name
                       (if (eq system-type 'windows-nt)
                           "Scripts/python.exe"
                         "bin/python")
                       venv-path)))
    (if (file-exists-p python-path)
        (progn
          ;; Set Python interpreter path
          (setq python-shell-interpreter python-path)

          ;; Update exec-path to include the venv's bin directory
          (let ((venv-bin-dir (file-name-directory python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          ;; Update PATH environment variable
          (setenv "PATH" (concat (file-name-directory python-path)
                                 path-separator
                                 (getenv "PATH")))

          ;; Update VIRTUAL_ENV environment variable
          (setenv "VIRTUAL_ENV" venv-path)

          ;; Remove PYTHONHOME if it exists
          (setenv "PYTHONHOME" nil)

          (message "Activated Python environment at %s" venv-path))
      (error "No Python environment found in %s" project-root))))

;;; Egglog
(define-derived-mode egglog-mode lisp-data-mode "Egglog"
  "Major mode for Egglog, derived from lisp-data-mode."
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local font-lock-defaults
              '((
                 ;; Keywords
                 ("\\_<\\(birewrite\\|constructor\\|calc\\|check\\|clear\\|clear-rules\\|datatype\\|declare\\|define\\|delete\\|extract\\|fail\\|function\\|include\\|input\\|let\\|panic\\|pop\\|print-stats\\|print-size\\|print-table\\|print\\|push\\|query\\|relation\\|repeat\\|rewrite\\|rule\\|run-schedule\\|run\\|set\\|sort\\|union\\)\\_>" . font-lock-keyword-face)
                 ;; Identifiers (variables)
                 ("\\<[a-zA-Z][a-zA-Z0-9_]*\\>" . font-lock-variable-name-face)
                 ;; Numbers (integers and floats)
                 ("-?[0-9]+\\(?:\\.[0-9]*\\)?" . font-lock-constant-face)
                 ;; Strings
                 ("\"[^\"]*\"" . font-lock-string-face)
                 ;; Comments
                 (";.*" . font-lock-comment-face)
                 ;; Builtins (symbols starting with :)
                 (":\\w+" . font-lock-builtin-face))))

  ;; Make `datatype` indent like `defun`
  (put 'datatype 'lisp-indent-function 'defun))
(add-to-list 'auto-mode-alist '("\\.egg\\'" . egglog-mode))

;;; markdown-mode
(use-package markdown-mode
  :ensure t
  :defer t)

;;; asciidoc-mode
;;; TODO change font faces
(use-package adoc-mode
  :ensure t)

;;; web-mode
(use-package web-mode
  :ensure t
  :defer t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.mako\\'" . web-mode)))

;;; MODELINE
(defsubst bk/mode-line-meow (active?)
  (when (bound-and-true-p meow-global-mode)
    (let* ((state (meow--current-state))
           (state-name (meow--get-state-name state))
           (ictr-face (alist-get state meow-indicator-face-alist))
           (ictr-face-new `(:inherit
                            ,ictr-face
                            :inverse-video
                            ,active?)))
      (if state-name
          (propertize
           (format " %s " state-name)
           'face ictr-face-new)
        ""))))

(defvar-local bk/mode-line-vcs-info nil
  "Variable holding mode-line VCS info.")

(defun bk/mode-line-update-vcs-info (&rest _)
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state (vc-state buffer-file-name backend))
           (state-sym (cond ((eq state 'up-to-date) "✓")
                            ((eq state 'edited) "*")
                            ((eq state 'added) "@")
                            ((eq state 'needs-update) "↓")
                            ((eq state 'needs-merge) "⟷")
                            ((eq state 'unlocked-changes) "")
                            ((eq state 'removed) "×")
                            ((eq state 'conflict) "!")
                            ((eq state 'missing) "?")
                            ((eq state 'ignored) "-")
                            ((eq state 'unregistered) "+")
                            ((stringp state) (concat "#" state ":"))
                            (t " ")))
           (face (cond ((eq state 'up-to-date) 'diff-added)
                       (t 'diff-removed)))
           (rev (substring-no-properties vc-mode 5)))
      (setq bk/mode-line-vcs-info (propertize (concat " [" rev " " state-sym "] " ) 'face face)))))

(add-hook 'find-file-hook #'bk/mode-line-update-vcs-info)
(add-hook 'after-save-hook #'bk/mode-line-update-vcs-info)
(advice-add #'vc-refresh-state :after #'bk/mode-line-update-vcs-info)
(advice-add #'rename-buffer :after #'bk/mode-line-update-vcs-info)
(advice-add #'set-visited-file-name :after #'bk/mode-line-update-vcs-info)

(defun bk/mode-line-buffer-name ()
  (if-let ((file (buffer-file-name))
           (proj (project-current)))
      (file-relative-name file (project-root proj))
    (buffer-name)))

(defsubst bk/mode-line-lhs ()
  (let* ((active? (mode-line-window-selected-p)))
    `((:eval (cond
              (buffer-read-only (propertize " Ω " 'face '(:weight light :inverse-video t)))
              ((buffer-modified-p) (propertize " Δ " 'face '(:weight light :inverse-video t)))
              (t (propertize " λ " 'face '(:weight light :inverse-video t)))))
      (:eval (bk/mode-line-meow ,active?))
      (,active? (:eval bk/mode-line-vcs-info))
      " "
      (:eval (propertize (bk/mode-line-buffer-name) 'face '(:weight bold)))
      " "
      "%4l:%3c")))

(defsubst bk/mode-line-rhs ()
  (let* ((active? (mode-line-window-selected-p)))
    `(" " mode-name " "
      (flymake-mode (,active? (:eval (flymake--mode-line-counters)))))))

(setq-default mode-line-format
              '((:eval (bk/mode-line-lhs))
                mode-line-format-right-align
                (:eval (bk/mode-line-rhs))))
