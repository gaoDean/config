(setq straight-check-for-modifications nil)
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

(use-package nano-emacs
      :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs")
      :no-require t
      :init
      (setq nano-font-family-monospaced "Input")
      (setq nano-font-family-proportional "Merriweather")
      (setq nano-font-size 20)
      :config
      (let ((inhibit-message t))
        (message "Welcome to GNU Emacs / N Λ N O edition")
        (message (format "Initialization time: %s" (emacs-init-time))))

      (require 'nano-layout)
      ;; (require 'nano-faces)
      (require 'nano-defaults)
      (require 'nano-session))


    (use-package nano-theme
      :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
      :config
      (nano-dark))

    (use-package nano-splash
      :after nano-theme
      :straight (nano-splash :type git :host github :repo "rougier/nano-splash")
      :config (nano-splash))

    (use-package nano-minibuffer
      :straight (nano-minibuffer :type git :host github :repo "rougier/nano-minibuffer"))

    ;; (use-package nano-command
    ;;   :straight (nano-command :type git :host github :repo "rougier/nano-command"))

    (use-package nano-modeline
      :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
      :config (nano-modeline-mode))

    ;; (require 'nano-theme)
    ;; (require 'nano-theme-dark)
    ;; (nano-theme-set-dark)
    ;; (nano-refresh-theme)

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'meta
        mac-use-title-bar nil))


    ;; (require 'nano-counsel)

  (setq default-frame-alist '((min-height . 1)  '(height . 45)
                              (min-width  . 1)  '(width  . 81)
                              (vertical-scroll-bars . nil)
                              (internal-border-width . 24)
                              (undecorated . t)
                              (tool-bar-lines . 0)
                              (menu-bar-lines . 1)))

  (setq initial-frame-alist default-frame-alist)

(run-with-timer 1.5 nil 'nano-splash)

(use-package mixed-pitch
  :hook text-mode)

(use-package all-the-icons)

(set-face-attribute 'default nil
                    :family "Input"
                    :weight 'light
                    :height 250)

(set-face-attribute 'bold nil
                    :family "Input"
                    :weight 'bold)

(set-face-attribute 'italic nil
                    :family "Input"
                    :weight 'semilight
                    :slant 'italic)

(set-face-attribute 'bold-italic nil
                    :family "Input"
                    :weight 'bold
                    :slant 'italic)

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
  :hook (org-mode . org-modern-mode)
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

(use-package org-autolist
  :after org
  :hook org-mode)

(use-package org-fragtog
  :after org
  :init
  (setq org-startup-with-latex-preview t
        org-latex-create-formula-image-program 'dvisvgm
        org-highlight-latex-and-related '(latex script entities))
        (plist-put org-format-latex-options :scale 2.6)
        :hook org-mode)

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package avy
  :after evil
  :config
  (leader-def '(normal visual) "j" 'avy-goto-char-2))

(use-package projectile
    :defer t
    :config
    (projectile-mode)
    (leader-def 'normal
        "p p" 'projectile-switch-project
        "p p" 'projectile-find-file))

(use-package helpful
  :config
  (leader-def 'normal
    "h f" 'helpful-callable
    "h v" 'helpful-variable
    "h k" 'helpful-key
    "h F" 'describe-face
    "h x" 'helpful-command
    "h ." 'helpful-at-point))

(use-package vertico
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

  :config
  (vertico-mode)
  (set-face-attribute 'vertico-group-separator nil
                      :strike-through t)
  (set-face-attribute 'vertico-current nil
                      :inherit '(nano-strong nano-subtle))
  (set-face-attribute 'completions-first-difference nil
                      :inherit '(nano-default))
  (keymap-set vertico-map "?" #'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous)
  (keymap-set vertico-map "M-TAB" #'minibuffer-complete))

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

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package yasnippet-snippets)
(use-package yasnippet
  :config (yas-global-mode 1))

(use-package magit
  :config
  (leader-def 'normal "g g" 'magit))

(use-package company
  :init
  (setq company-backends '((company-capf company-yasnippet company-semantic company-keywords company-dabbrev-code)))
  :hook (after-init . global-company-mode))
(use-package company-box
  :hook (company-mode . company-box-mode))

(if (string-equal system-type "darwin")
   (setq dired-use-ls-dired nil))
 (use-package dirvish
      :straight (dirvish :type git :host github :repo "isamert/dirvish")
    :init
    (dirvish-override-dired-mode)
    :custom
    (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
     '(("h" "~/"                          "Home")
       ("d" "~/Downloads/"                "Downloads")
       ("v" "~/vau/"                      "vau")
       ("r" "~/repos/"                    "repos")
       ("t" "~/.Trash"                    "Trash")))
    :config
    ;; (dirvish-peek-mode) ; Preview files in minibuffer
    ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
    (setq dirvish-mode-line-format
          '(:left (sort symlink) :right (omit yank index)))
    (setq dirvish-attributes
          '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
    (setq delete-by-moving-to-trash t)
    (setq dired-listing-switches
          "-l --almost-all --human-readable --group-directories-first --no-group"))

(general-def 'dired-mode-map
  "% l"   'dired-downcase
  "% m"   'dired-mark-files-regexp
  "% u"   'dired-upcase
  "* %"   'dired-mark-files-regexp
  "* ."   'dired-mark-extension
  "* /"   'dired-mark-directories
  "+"     'dired-create-directory
  "-"     'dirvish-narrow
  "<tab>" 'dirvish-toggle-subtree
  "M"     'dirvish-mark-menu
  "S"     'dirvish-symlink
  "a"     'dirvish-quick-access
  "c"     'dirvish-chxxx-menu
  "d"     'dired-do-delete
  "x"     'dired-do-delete
  "f"     'dirvish-file-info-menu
  "h"     'dired-up-directory
  "l"     'dired-open-file
  "m"     'dired-mark
  "p"     'dirvish-yank
  "r"     'dired-do-rename
  "t"     'dirvish-new-empty-file-a
  "u"     'dired-unmark
  "v"     'dirvish-move
  "y"     'dirvish-yank-menu
  "z"     'dired-do-compress)

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

(defun dg/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(leader-def :keymaps 'normal

            ;; misc
            "."   'find-file

            ;; buffers
            "b b" 'switch-to-buffer
            "b k" 'kill-some-buffers
            "b k" 'kill-this-buffer
            "b n" 'evil-next-buffer
            "b p" 'evil-prev-buffer

            ;; windows
            "w w" 'evil-window-next
            "w k" 'evil-window-up
            "w j" 'evil-window-down
            "w h" 'evil-window-left
            "w l" 'evil-window-right
            "w p" 'evil-window-mru
            "w c" 'evil-window-close
            "w v" 'evil-window-vsplit
            "w >" 'evil-window-increase-width
            "w <" 'evil-window-decrease-width
            "w +" 'evil-window-increase-height
            "w -" 'evil-window-decrease-height

            ;; file
            "f r" 'recentf

            ;; emacs
            "e r" 'dg/reload-init-file)

(general-define-key :states '(normal visual)
            "g j" 'evil-next-visual-line
            "g j" 'evil-previous-visual-line)

(general-define-key "C-v" 'evil-paste-after)

(general-define-key (kbd "C-x C-m") 'execute-extended-command)
