(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'cl-lib)
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

(defun initfile () (interactive)
   (find-file (concat user-emacs-directory "/emacs.org")))

(defun reconfigure () (interactive)
   (load-file (concat user-emacs-directory "/init.el")))

(defun bk/split-window-sensibly (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
    i.e. windows
tiled side-by-side."
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

(defun bk/backup-file-name (fpath)
  "Return a new file path of a given file path.
  If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(use-package hydra
  :ensure t
  :init
  (defhydra hydra-goto (:color blue)
    ("l" consult-goto-line "Line")
    ("f" consult-flymake "Flymake Error"))
  :config
  (keymap-global-set "C-x g" 'hydra-goto/body))

(use-package emacs
  :ensure nil
  :init
  (setopt inhibit-startup-screen t
          inhibit-startup-message t
          ;;debug-on-error t
          inhibit-startup-echo-area-message "bhargavkk"
          visible-bell t
          display-time-default-load-average nil
          sentence-end-double-space nil
          make-backup-file-name-function 'bk/backup-file-name
          mouse-wheel-tilt-scroll t
          mouse-wheel-flip-direction t
          require-final-newline t
          tab-always-indent 'complete
          indicate-buffer-boundaries 'left
          use-short-answers t
          save-interprogram-paste-before-kill t
          history-length 25
          read-buffer-completion-ignore-case t
          read-file-name-completion-ignore-case t
          find-file-suppress-same-file-warnings t
          scroll-preserve-screen-position t
          backward-delete-char-untabify-method 'nil
          split-height-threshold 4
          split-width-threshold 80
          split-window-preferred-function 'bk/split-window-sensibly
          display-line-numbers-width 3
          auto-revert-avoid-polling t
          auto-revert-interval 5
          auto-revert-verbose nil
          auto-revert-check-vc-info t
          global-auto-revert-non-file-buffers t
          recentf-max-saved-items 1000
          recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                                (lambda (file) (file-in-directory-p file package-user-dir)))
          uniquify-buffer-name-style 'forward
          window-divider-default-places t
          window-divider-default-bottom-width 1
          window-divider-default-right-width 1
          completion-cycle-threshold 1 ; TAB cycles candidates
          completions-detailed t ; Show annotations
          completion-styles '(basic initials substring)
          completion-auto-help 'always ; Always open completion
          completions-max-height 20 ; Set arbitrary max height
          completions-format 'one-column ; One-column display
          completions-group t ; Group completions
          completion-auto-select 'second-tab)


  (when (boundp 'read-extended-command-predicate)
    (setopt read-extended-command-predicate
            #'command-completion-default-include-p))

  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (add-hook 'prog-mode-hook #'electric-pair-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'window-setup-hook #'window-divider-mode)

  (setq-default major-mode 'text-mode
                fill-column 80
                tab-width 4
                indent-tabs-mode nil
                cursor-in-non-selected-windows t
                bidi-display-reordering nil
                create-lockfiles nil
                word-wrap t
                cursor-type 'bar)

  :config
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (global-auto-revert-mode)
  (savehist-mode)
  (pixel-scroll-precision-mode)
  (set-fringe-mode 10)
  (delete-selection-mode t)
  (global-so-long-mode t)
  (recentf-mode t)
  (save-place-mode t)

  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)))

  :bind (("<escape>" . keyboard-escape-quit)
         ("C-x C-z" . nil)
         ("C-<wheel-up>" . nil)
         ("C-<wheel-down>" . nil)
         ("C-x C-r" . recentf)
         :map minibuffer-mode-map
         ("TAB" . minibuffer-complete)))

(use-package eshell
  :ensure nil
  :defer t
  :config (add-hook 'eshell-preoutput-filter-functions
                    'ansi-color-filter-apply))

(use-package which-key
  :ensure nil
  :config (which-key-mode))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode)
  (setopt vertico-buffer-display-action '(display-buffer-in-direction
                                          (direction . right)
                                          (window-width . 0.3))))

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
  :after vertico
  :init
  (marginalia-mode))

;; Todo: make a goto hydra
(use-package consult
  :after vertico
  :ensure t
  :bind   (([remap switch-to-buffer] . consult-buffer)
           ("C-x C-b" . consult-buffer)
           ("C-x i" . consult-imenu)
           ([remap yank-pop]   . consult-yank-pop)
           ("C-s" . consult-line)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("C-x o" . consult-outline))
  :config
  (setq vertico-multiform-commands
        `((consult-grep buffer ,(lambda (_) (text-scale-set -1)))
          (consult-ripgrep buffer ,(lambda (_) (text-scale-set -1)))
          (consult-line buffer ,(lambda (_) (text-scale-set -1)))
          (consult-flymake buffer ,(lambda (_) (text-scale-set -1)))
          (consult-imenu buffer ,(lambda (_) (text-scale-set -1)))
          (consult-outline buffer ,(lambda (_) (text-scale-set -1)))))
  (setq consult-narrow-key "<"))

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
  :after grid
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

(use-package transient
  :ensure t)

(use-package magit
  :after transient
  :ensure t
  :config
  (global-unset-key (kbd "C-x g")))

(use-package diff-hl
  :ensure t
  :hook (prog-mode text-mode)
  :config
  (set-face-attribute 'diff-hl-insert nil :foreground "#eeffee")
  (set-face-attribute 'diff-hl-delete nil :foreground "#ffeeee")
  (set-face-attribute 'diff-hl-change nil :foreground "#ddddff"))

(use-package scratch
  :ensure t
  :bind (("C-c s" . scratch)))

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
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode


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
  (setq popper-window-height 0.33)
  :config
  (setopt popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis)))
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package rg
  :ensure t)

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
    ("!" project-shell-command "Run Command")
    ("c" consult-ripgrep "Consult Ripgrep" :column "Search")
    ("r" project-rg "rg")))

(use-package racket-mode
  :defer t
  :ensure t)

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

(use-package python
  :ensure nil
  :defer t
  :after hydra
  :bind (:map python-mode-map
              ("C-x l" . hydra-python/body))
  :init
  (defhydra hydra-python (:color blue)
    ("v" venv-activate "Start venv")))

(use-package apheleia
  :ensure t
  :defer t
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yaml-imenu
  :ensure t
  :defer t
  :after yaml-mode
  :config (yaml-imenu-enable))

(use-package expreg
  :ensure t)

(use-package markdown-mode
  :ensure t)

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
  (fset #'jsonrpc--log-event #'ignore)
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))
  (add-to-list 'eglot-server-programs
               '(racket-mode . ("racket" "-l" "racket-langserver")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(use-package flymake
  :ensure nil
  :config
  (set-face-attribute 'flymake-error nil :underline '(:style line :color "red"))
  (set-face-attribute 'flymake-note nil :underline '(:style line :color "green"))
  (set-face-attribute 'flymake-warning nil :underline '(:style line :color "blue")))

(when (member "Iosevka Clear Type" (font-family-list))
  (set-face-attribute 'variable-pitch nil :font "Iosevka Clear Type" :height 170))

(when (member "Iosevka Clear" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka Clear" :height 170)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Clear" :height 170))

(use-package mixed-pitch
  :ensure t
  :after enlight
  :hook ((enlight-mode org-mode text-mode) . mixed-pitch-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(set-face-attribute 'mode-line nil
                    :box nil)

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

(use-package surround
  :ensure t)

(use-package avy
  :ensure t
  :after hydra
  :bind ("C-x a" . hydra-avy/body)
  :init
  (defhydra hydra-avy (:exit t :hint nil)
    "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("c" avy-goto-char-timer)
    ("C" avy-goto-char)
    ("w" avy-goto-word-1)
    ("W" avy-goto-word-0)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region)))

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
   '("S" . surround-insert)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . completion-at-point)
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
   '("Z" . hydra-avy/body)
   '("?" . meow-cheatsheet)
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

(set-face-attribute 'mode-line nil
                    :background "#000000"
                    :foreground "#ffffff"
                    :box '(:line-width 3 :color "#000000"))
(set-face-attribute 'mode-line-inactive nil
                    :background "#878a8b"
                    :foreground "#ffffff"
                    :box '(:line-width 3 :color "#878a8b"))
(set-face-attribute 'org-block nil
                    :inherit 'default)
(set-face-attribute 'font-lock-string-face nil
                    :foreground "#878a8b")
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#cbcdcd"
                    :slant 'italic)
(set-face-attribute 'font-lock-doc-face nil
                    :foreground "#878a8b")
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground "#00000")
(set-face-attribute 'font-lock-keyword-face nil
                    :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil
                    :weight 'bold)
(set-face-attribute 'org-document-title nil
                    :height 2.0)
;; Resize Org headings
(dolist (face '(org-level-1
                org-level-2
                org-level-3
                org-level-4
                org-level-5
                org-level-6
                org-level-7
                org-level-8))
  (set-face-attribute face nil :inherit 'variable-pitch :height 1.2))

(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
