(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defvar elpaca-installer-version 0.10)
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
;; Enable use-package :ensure support for Elpaca.
(elpaca-use-package-mode))

(use-package expreg
  :ensure t)

(defun initfile () (interactive)
   (find-file (concat user-emacs-directory "/emacs.org")))

(defun reconfigure () (interactive)
   (load-file (concat user-emacs-directory "/init.el")))

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
   '("-" . meow-reverse)
   '(";" . comment-dwim)
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
   '("P" . consult-yank-pop)
   '("q" . meow-quit)
   '("Q" . consult-goto-line)
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
   '("=" . expreg-expand)
   '("<up>" . windmove-up)
   '("<down>" . windmove-down)
   '("<left>" . windmove-left)
   '("<right>" . windmove-right)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :custom-face
  (meow-normal-indicator ((t (:foreground "#ffffff" :background "#2e943a"))))
  (meow-insert-indicator ((t (:foreground "#ffffff" :background "#b56227"))))
  (meow-motion-indicator ((t (:foreground "#ffffff" :background "#542690"))))
  (meow-keypad-indicator ((t (:foreground "#ffffff" :background "#b59944"))))
  (meow-beacon-indicator ((t (:foreground "#ffffff" :background "#0949ac"))))
  :config
  (setopt meow-keypad-leader-dispatch "C-x")
  (setq-default meow-replace-state-name-list
                '((normal . "NOR")
                  (motion . "MTN")
                  (keypad . "KPD")
                  (insert . "INS")
                  (beacon . "BCN")))
  (meow-setup)
  (meow-global-mode 1))

(defun bk/split-window-sensibly (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
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
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
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

(defun backup-file-name (fpath)
  "Return a new file path of a given file path. If the new path's
  directories does not exist, create them."
  (let* ((backup-root-dir (concat user-emacs-directory "emacs-backup/"))
         (file-path (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backup-file-path
          (replace-regexp-in-string "//"
                                    "/"
                                    (concat backup-root-dir file-path "~"))))
    (make-directory (file-name-directory backup-file-path)
                    (file-name-directory backup-file-path)) backup-file-path))

(setopt inhibit-startup-screen             t ; Disable the startup screen
        inhibit-startup-message            t ; Suppress the startup message in
                                        ; the *Messages*
        inhibit-startup-echo-area-message  "bhargavkk") ; Suppress the startup
                                        ; message

(use-package flymake
  :ensure t
  :defer t
  :config
  (set-face-attribute 'flymake-error nil :underline '(:style line :color "red"))
  (set-face-attribute 'flymake-note nil :underline '(:style line :color "green"))
  (set-face-attribute 'flymake-warning nil :underline '(:style line :color "blue")))

(use-package emacs
  :ensure nil
  :init
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
  (set-face-attribute 'font-lock-builtin-face nil :weight 'bold)

  (setopt visible-bell                          t ; Don't want Emacs making
                                        ; noise
          display-time-default-load-average     nil ; Don't show load time in
                                        ; the mode line
          sentence-end-double-space             nil ; Don't require double
                                        ; spaces after sentence
          make-backup-file-name-function        'backup-file-name ; Clean backup
                                        ; files
          mouse-wheel-tilt-scroll               t ; Enable horizontal scrollin
          mouse-wheel-flip-direction            t ; Reverse mouse wheel scroll
          column-number-mode                    t ; Show the current column
                                        ; number
          size-indication-mode                  t ; Show the size of the buffer
                                        ; in the modeline
          require-final-newline                 t ; Always ensure files end with
                                        ; a newline
          tab-always-indent                     'complete
          indicate-buffer-boundaries            'left ; Show buffer boundaries
                                        ; on the left side
          use-short-answers                     t ; Use 'y'/'n' instead of
                                        ; 'yes'/'no' for yes/no
                                        ; prompts
          save-interprogram-paste-before-kill   t ; Save text to the clipboard
                                        ; before killing it
          history-length                        2 ; Limit the saved minibuffer
                                        ; history to the last 25
                                        ; entries
          read-buffer-completion-ignore-case    t ; Ignore case when completing
                                        ; buffer names
          read-file-name-completion-ignore-case t ; Ignore case when completing
                                        ; file names
          find-file-suppress-same-file-warnings t ; Just redirect to the
                                        ; existing buffer
          scroll-preserve-screen-position t
          backward-delete-char-untabify-method  'nil) ; Make backspace remove
                                        ; entire indents

  (setq split-height-threshold 4
        split-width-threshold 80
        split-window-preferred-function 'bk/split-window-sensibly)

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

  (when (member "Iosevka Clear Type" (font-family-list))
    (set-face-attribute 'variable-pitch nil :font "Iosevka Clear Type" :height 170))

  (when (member "Iosevka Clear" (font-family-list))
    (set-face-attribute 'default nil :font "Iosevka Clear" :height 170)
    (set-face-attribute 'fixed-pitch nil :font "Iosevka Clear" :height 170))

  :config
  (tooltip-mode -1)      ; Remove tooltips
  (blink-cursor-mode -1) ; Stop cursor from blinking

  :hook ((before-save . delete-trailing-whitespace)) ; Delete trailing whitespaces

  :bind (("<escape>" . keyboard-escape-quit) ; Quit on escape
         ("C-x C-z" . nil)                   ; Unset suspend-frame
         ("C-<wheel-up>" . nil)              ; Unset text scale up
         ("C-<wheel-down>" . nil)            ; Unset text scale down
         ("C-x C-r" . recentf)
         :map minibuffer-mode-map
         ("TAB" . minibuffer-complete)))     ; Minibuffer completion on tab

(use-package mixed-pitch
  :ensure t
  :after enlight
  :hook ((enlight-mode org-mode text-mode) . mixed-pitch-mode))

;; Automatch brackets
(use-package electric-pair
  :ensure nil
  :hook   prog-mode)

;; Display line numbers
(use-package display-line-numbers
  :ensure nil
  :hook   (prog-mode conf-mode)
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
  :ensure nil
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
  :init   (setopt window-divider-default-places       t   ; Enable window
                                        ; dividers in Emacs
                  window-divider-default-bottom-width 1   ; Set the width of the
                                        ; divider at the
                                        ; bottom to 1
                  window-divider-default-right-width  1)) ; Set the width of the
                                        ; divider on the right
                                        ; side to 1

;; Minibuffer enhancements
(use-package minibuffer
  :ensure nil
  :init
  (setopt completion-cycle-threshold   1 ; TAB cycles candidates
          completions-detailed         t ; Show annotations
          completion-styles            '(basic initials substring) ; Styles to
                                        ; match input
                                        ; to
                                        ; candidates
          completion-auto-help         'always ; Always open completion
          completions-max-height       20 ; Set arbitrary max height
          completions-format           'one-column ; One-column display
          completions-group            t ; Group completions
          completion-auto-select       'second-tab)) ; Eagerly auto-select on second TAB

(use-package which-key
  :ensure nil
  :config (which-key-mode))

(use-package exec-path-from-shell
  :ensure t
  :if     (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode)
  (setopt vertico-buffer-display-action '(display-buffer-in-direction
                                          (direction . right)
                                          (window-width . 0.3)))
  (setq vertico-multiform-commands
        `((consult-imenu buffer ,(lambda (_) (text-scale-set -1)))
          (consult-outline buffer ,(lambda (_) (text-scale-set -1))))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind   (([remap switch-to-buffer] . consult-buffer)   ; orig. switch-to-buffer
           ("C-x C-b" . consult-buffer)
           ("C-x i" . consult-imenu)
           ([remap yank-pop]   . consult-yank-pop) ; orig. yank-pop
           ("C-s" . consult-line)       ; Alternative: rebind C-s to use
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("C-x o" . consult-outline))
  :config
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :demand t
  :bind (("C-x ." . embark-act)))

(use-package embark-consult
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package grid
  :ensure (:host github :repo "ichernyshovvv/grid.el"))

(defface enlight-violet
  '((t (:foreground "purple" :width expanded)))
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
    (grid-get-box `(:align center :content ,enlight-emacs  :width 80))
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
(setq-default olivetti-body-width 120)

(use-package auto-olivetti
  :ensure (:repo "https://codeberg.org/ashton314/auto-olivetti")
  :config
  (setopt auto-olivetti-enabled-modes '(text-mode conf-mode prog-mode))
  (auto-olivetti-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(set-face-attribute 'mode-line nil
                    :box nil)

(defun vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let* ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                          (substring rev 0 7))))
    branch))

(defun vc-branch ()
  '(:eval
    (when-let* (((mode-line-window-selected-p))
                (file (buffer-file-name))
                (backend (vc-backend file)))
      (vc-branch-name file backend))))

(setq-default mode-line-format
              '("%e"
                (:eval (meow-indicator))
                " "
                (:eval (propertize (buffer-name) 'face 'bold))
                " | "
                (:eval (propertize "[%m]" 'face '(:weight light)))
                (:eval (cond
                        (buffer-read-only (propertize " [Ω] " 'face '(:weight light)))
                        ((buffer-modified-p) (propertize " [Δ] " 'face '(:weight light)))
                        (t (propertize " [λ] " 'face '(:weight light)))))
                (:eval (when vc-mode
                       (propertize (concat "[" (substring vc-mode 5) "]") 'face '(:weight light))))
                (:eval (propertize " %4l:%3c" 'face '(:weight light)))))

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode))

(use-package transient
  :ensure t)
(use-package magit
  :after transient
  :ensure t
  :bind (:map global-map
              ("C-x g" . magit-status)))

(use-package scratch
  :ensure t
  :bind (("C-c s" . scratch)))

(use-package popper
  :custom
  (popper-group-function #'popper-group-by-directory)
  (popper-echo-dispatch-actions t)
  :bind (:map popper-mode-map
              ("C-x `"     . popper-toggle)
              ("C-M-`"   . popper-cycle)
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
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*$" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*.*eshell.*\\*.*$"
          "^\\*.*shell.*\\*.*$"
          "^\\*.*terminal.*\\*.*$"
          "^\\*.*vterm[inal]*.*\\*.*$"
          "^\\*eat\\*.*$"
          "^\\*.*-eat\\*.*$"

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
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package project
  :ensure nil
  :bind (:map project-prefix-map
              ("m" . magit-project-status))
  :config
  (setq project-switch-commands '((project-find-file "find file")
                                  (project-find-regexp "find regexp")
                                  (project-dired "dired")
                                  (project-eshell "eshell")
                                  (magit-project-status "magit")
                                  (shell "shell"))))

(use-package diff-hl
  :ensure t
  :hook (prog-mode text-mode)
  :config
  (set-face-attribute 'diff-hl-insert nil :foreground "#eeffee")
  (set-face-attribute 'diff-hl-delete nil :foreground "#ffeeee")
  (set-face-attribute 'diff-hl-change nil :foreground "#ddddff")
  )

(use-package racket-mode
  :defer t
  :ensure t)

(defun uv-activate ()
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

          (message "Activated UV Python environment at %s" venv-path))
      (error "No UV Python environment found in %s" project-root))))

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (apheleia-global-mode +1))

(use-package eglot
  :ensure nil
  :defer t
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (setq eglot-highlight-symbol nil)
  (fset #'jsonrpc--log-event #'ignore)
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))
  (add-to-list 'eglot-server-programs
               '(racket-mode . ("racket" "-l" "racket-langserver")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(defvar eglot-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'eglot)
    (define-key map "a" 'eglot-code-actions)
    (define-key map "o" 'eglot-code-action-organize-imports)
    (define-key map "r" 'eglot-rename)
    (define-key map "k" 'eglot-shutdown)
    (define-key map "f" 'eglot-format)
    map)
  "Eglot keymap")
(global-set-key (kbd "C-x e") eglot-map)

(global-unset-key (kbd "C-x o"))
(use-package combobulate
  :ensure (:repo "https://github.com/mickeynp/combobulate.git")
  :custom
  (combobulate-key-prefix "C-x o")
  :hook ((prog-mode . combobulate-mode)))

(use-package poly-rst
  :ensure t)
