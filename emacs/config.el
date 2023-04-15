(setq straight-check-for-modifications (list 'check-on-save 'find-when-checking))
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq straight-host-usernames
      '((github . "gaoDean")
        (gitlab . "gaoDean")))

(straight-use-package 'benchmark-init)
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(use-package general)
(general-create-definer leader-def
                        ;; :prefix my-leader
                        :prefix "SPC")

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer t
        evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :hook org-mode)

(use-package evil-numbers
  :config
  (leader-def :keymaps '(normal visual) "a +" 'evil-numbers/inc-at-pt)
  (leader-def :keymaps '(normal visual) "a -" 'evil-numbers/dec-at-pt))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :custom-face
  (default ((t (:family "Input Mono" :height 240))))
  (italic ((t (:family "Input Mono" :height 240 :slant italic))))
  (variable-pitch ((t (:family "Lato" :height 240))))
  :config
  (nano-dark))

(use-package nano-splash
  :custom
  (nano-splash-duration 20)
  :straight (nano-splash :type git :host github :repo "gaoDean/nano-splash")
  :config (nano-splash))

(add-to-list 'load-path "~/.config/emacs/plugins/")
(with-eval-after-load 'nano-splash
  (require 'nano-session))

(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default header-line-format
              '((:eval
                 (mode-line-render
                  (format-mode-line (list
                                     (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                                                 'help-echo "Mode(s) menu"
                                                 'mouse-face 'mode-line-highlight
                                                 'local-map   mode-line-major-mode-keymap)
                                     " %b "
                                     (if (and buffer-file-name (buffer-modified-p))
                                         (propertize "[M]" 'face `(:inherit face-faded)))))
                  (format-mode-line (propertize "%4l:%2c  " 'face `(:inherit face-faded)))))))
(setq-default mode-line-format nil)


(defun dg/set-elegant-header-faces (&rest r)
  (set-face-attribute 'header-line nil
                      :underline nil
                      :background nil))

(with-eval-after-load 'nano-theme (dg/set-elegant-header-faces))
(advice-add 'nano-dark :after #'dg/set-elegant-header-faces)
(advice-add 'nano-light :after #'dg/set-elegant-header-faces)

(setq user-full-name "Dean Gao"
      user-mail-address "gao.dean@hotmail.com")

(defun display-startup-echo-area-message ()
  (message ""))

(use-package mixed-pitch
  :hook text-mode)

(use-package all-the-icons
  :hook (pre-command . (lambda())))

(use-package writeroom-mode
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-header-line t)
  :general
  (leader-def :keymaps 'normal "t w" 'writeroom-mode))

(setq-default fill-column 80                          ; Default line width
                sentence-end-double-space nil           ; Use a single space after dots
                bidi-paragraph-direction 'left-to-right ; Faster
                truncate-string-ellipsis "…")           ; Nicer ellipsis

  (setq x-underline-at-descent-line nil
        x-use-underline-position-properties t
        underline-minimum-offset 10)

  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'prog-mode-hook 'visual-line-mode)

(setq-default indent-tabs-mode nil        ; Stop using tabs to indent
              tab-always-indent 'complete ; Indent first then try completions
              tab-width 4)                ; Smaller width for tab characters

;; Let Emacs guess Python indent silently
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

(use-package org :straight (:type built-in))

(use-package org-modern
  :init
  (setq
   org-catch-invisible-edits 'show-and-error
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
   org-modern-label-border 0.3
   org-modern-hide-stars " "
   line-spacing 0.1
   org-pretty-entities t
   org-ellipsis "…")
  :hook org-mode
  :config
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-level-1 nil :font "Source Sans Pro" :weight 'bold :height 1.4)
    (set-face-attribute 'org-level-2 nil :font "Source Sans Pro" :weight 'bold :height 1.3)
    (set-face-attribute 'org-level-3 nil :font "Source Sans Pro" :weight 'bold :height 1.2)
    (set-face-attribute 'org-level-4 nil :font "Source Sans Pro" :weight 'bold :height 1.1)
    (set-face-attribute 'org-level-5 nil :font "Source Sans Pro" :weight 'bold :height 1.0)
    (set-face-attribute 'org-level-6 nil :font "Source Sans Pro" :weight 'bold :height 1.0)
    (set-face-attribute 'org-level-7 nil :font "Source Sans Pro" :weight 'bold :height 1.0)
    (set-face-attribute 'org-modern-symbol nil :font "FiraCode NF" :height 1.1)))

(use-package org-autolist :hook org-mode)

(use-package org-fragtog
  :init
  (setq org-startup-with-latex-preview t
        org-latex-create-formula-image-program 'dvisvgm
        org-highlight-latex-and-related '(latex script entities))
  :config
  (plist-put org-format-latex-options :scale 2.6)
  :hook org-mode)

(use-package org-appear
  :hook org-mode
  :custom
  (org-appear-autoentities t)
  (org-appear-autolinks t))

(use-package org-auto-tangle :hook org-mode)

(with-eval-after-load 'org
  (setq org-latex-pdf-process (list "latexmk -f -pdfxe -interaction=nonstopmode -output-directory=%o %f")
        org-latex-default-packages-alist
        '(("AUTO" "inputenc" nil
          ("pdflatex"))
         ("T1" "fontenc" nil
          ("pdflatex"))
         ("" "graphicx" t)
         ("" "longtable" t)
         ("" "wrapfig" nil)
         ("" "rotating" nil)
         ("normalem" "ulem" t)
         ("" "amsmath" t)
         ("" "amssymb" t)
         ("" "capt-of" nil)
         ("" "hyperref" t))))

(with-eval-after-load 'ox-latex
  (defun get-string-from-file (filePath)
    "Return file content as string."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  (add-to-list 'org-latex-classes
               '("orgox"
                 "
                \\documentclass[hidelinks]{article}
                [DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package citeproc)

(use-package avy
  :custom
  (avy-keys '(?i ?s ?r ?t ?g ?p ?n ?e ?a ?o))
  :general
  (leader-def '(normal visual) "j" 'avy-goto-char-2)
  (general-define-key "C-j" 'avy-goto-char-2))

(use-package helpful
  :general
  (leader-def 'normal
    "h F" 'describe-face
    "h p" 'describe-package
    "h f" 'helpful-callable
    "h b" 'describe-bindings
    "h v" 'helpful-variable
    "h k" 'helpful-key
    "h x" 'helpful-command
    "h ." 'helpful-at-point))

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t)
  :hook pre-command
  :config
  (which-key-setup-side-window-right))

(setq ido-ignore-buffers '("^ " "\*"))

(use-package rg
  :commands (rg)
  :config
  (rg-enable-menu))

(use-package magit
  :general
  (leader-def 'normal "g g" 'magit))

(use-package projectile
    :config
    (projectile-mode)
    :general
    (leader-def 'normal
        "p p" 'projectile-switch-project
        "p g" 'projectile-ripgrep
        "SPC" 'projectile-find-file))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (setq vertico-resize nil        ; How to resize the Vertico minibuffer window.
        vertico-count 8           ; Maximal number of candidates to show.
        vertico-count-format nil)

  (setq vertico-grid-separator
        #("  |  " 2 3 (display (space :width (1))
                               face (:background "#ECEFF1")))

        vertico-group-format
        (concat #(" " 0 1 (face vertico-group-title))
                #(" " 0 1 (face vertico-group-separator))
                #(" %s " 0 4 (face vertico-group-title))
                #(" " 0 1 (face vertico-group-separator
                                display (space :align-to (- right (-1 . right-margin) (- +1)))))))
  (vertico-mode)

  :config
  (set-face-attribute 'vertico-group-separator nil
                      :strike-through t)
  (set-face-attribute 'vertico-current nil
                      :inherit '(nano-strong nano-subtle))
  (set-face-attribute 'completions-first-difference nil
                      :inherit '(nano-default))
  :general
  (:keymaps 'vertico-map
            "?" 'minibuffer-completion-help
            "M-RET" 'minibuffer-force-complete-and-exit
            "C-j" 'vertico-next
            "C-k" 'vertico-previous
            "M-TAB" 'minibuffer-complete))

(use-package savehist
  :config
  (savehist-mode))

(use-package marginalia
  :init
  (setq-default marginalia--ellipsis "…"    ; Nicer ellipsis
                marginalia-align 'right     ; right alignment
                marginalia-align-offset -1) ; one space on the right
  :config
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(substring orderless basic)
        orderless-component-separator 'orderless-escapable-split-on-space
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :hook (emacs-startup . global-corfu-mode)
  :init
  (setq tab-always-indent 'complete)
  :config
  (defun corfu-enable-always-in-minibuffer ()
    (unless (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil) 
      (corfu-mode 1))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (keymap-set corfu-map "M-q" #'corfu-quick-complete)
  (keymap-set corfu-map "C-q" #'corfu-quick-insert)
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first t) ;; Always preselect the prompt
  (corfu-echo-delay '(1.0 0.5))
  :general
  (:keymaps 'corfu-map
            "TAB" 'corfu-next
            "S-TAB" 'corfu-previous))

(use-package tempel
  :config
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection :after tempel)

(use-package shrink-path)
(use-package eshell-vterm
  :config
  (defalias 'eshell/v 'eshell-exec-visual))
(use-package eshell-up)

(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

(add-hook 'eshell-mode-hook (lambda ()
  (eshell/alias "f" "find-file $1")
  (eshell/alias "l" "ls -lh $*")
  (eshell/alias "la" "ls -alh $*")
  (eshell/alias "gs" "magit-status $*")
  (eshell/alias "g" "magit $*")
  (eshell/alias "d" "dirvish $*")))

(setq eshell-prompt-regexp "^.* λ "
      eshell-prompt-function #'+eshell/prompt)

(defun +eshell/prompt ()
  (let ((base/dir (shrink-path-prompt default-directory)))
        (concat (propertize (car base/dir)
                            'face 'font-lock-comment-face)
                (propertize (cdr base/dir)
                            'face 'font-lock-constant-face)
                (propertize (+eshell--current-git-branch)
                            'face 'font-lock-function-name-face)
                (propertize " λ" 'face 'eshell-prompt-face)
                ;; needed for the input text to not have prompt face
                (propertize " " 'face 'default))))

;; for completeness sake
(defun +eshell--current-git-branch ()
    (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
                             when (string-match "^\*" match)
                             collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))

(use-package dired-open)

(use-package dirvish
  :hook (emacs-startup . dirvish-override-dired-mode)
  :after dired-open
  :straight (dirvish :type git :host github :repo "isamert/dirvish")
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("v" "~/vau/"                      "vau")
     ("r" "~/repos/"                    "repos")
     ("t" "~/.Trash"                    "Trash")))
  :init
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index))
        dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg)
        delete-by-moving-to-trash t
        insert-directory-program "gls"
        dired-use-ls-dired t
        dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :config
  (evil-define-key 'normal dired-mode-map
    (kbd "% l") 'dired-downcase
    (kbd "% m") 'dired-mark-files-regexp
    (kbd "% u") 'dired-upcase
    (kbd "* %") 'dired-mark-files-regexp
    (kbd "* .") 'dired-mark-extension
    (kbd "* /") 'dired-mark-directories
    (kbd "+") 'dired-create-directory
    (kbd "-") 'dirvish-narrow
    (kbd "<tab>") 'dirvish-toggle-subtree
    (kbd "M") 'dirvish-mark-menu
    (kbd "S") 'dirvish-symlink
    (kbd "a") 'dirvish-quick-access
    (kbd "c") 'dirvish-chxxx-menu
    (kbd "d") 'dired-do-delete
    (kbd "x") 'dired-do-delete
    (kbd "f") 'dirvish-file-info-menu
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-open-file
    (kbd "m") 'dired-mark
    (kbd "p") 'dirvish-yank
    (kbd "r") 'dired-do-rename
    (kbd "t") 'dirvish-new-empty-file-a
    (kbd "u") 'dired-unmark
    (kbd "v") 'dirvish-move
    (kbd "y") 'dirvish-yank-menu
    (kbd "z") 'dired-do-compress))

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

(set-register ?c (cons 'file "~/.config/emacs/config.org"))

(defun dg/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(leader-def :keymaps 'normal
            ;; misc
            "."   'find-file

            ;; buffers
            "b b" 'ido-switch-buffer
            "b B" 'bs-show
            "b K" 'ido-kill-buffer
            "b k" 'kill-this-buffer
            "b n" 'bs-cycle-next
            "b p" 'bs-cycle-previous

            ;; windows
            "w w" 'evil-window-next
            "w k" 'evil-window-up
            "w j" 'evil-window-down
            "w h" 'evil-window-left
            "w l" 'evil-window-right
            "w p" 'evil-window-mru
            "w c" 'evil-window-delete
            "w v" 'evil-window-vsplit
            "w >" 'evil-window-increase-width
            "w <" 'evil-window-decrease-width
            "w +" 'evil-window-increase-height
            "w -" 'evil-window-decrease-height

            ;; open
            "o e" 'eshell

            ;; toggle
            ;; "t w" 'writeroom-mode

            ;; actions
            "a e" 'org-export-dispatch

            ;; file
            "f r" 'recentf

            ;; emacs
            "e r" 'dg/reload-init-file
            "e m" 'toggle-frame-maximized
            "e b t" 'benchmark-init/show-durations-tabulated
            "e b r" 'benchmark-init/show-durations-tree
            "r" 'jump-to-register)

(general-define-key :states '(normal visual)
            "g j" 'evil-next-visual-line
            "g j" 'evil-previous-visual-line
            "C-u" 'evil-scroll-up)

(general-define-key "C-v" 'evil-paste-after)

(general-define-key (kbd "C-x C-m") 'execute-extended-command)
