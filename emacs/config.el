;; (setq straight-check-for-modifications nil)
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

(setq mac-option-key-is-meta t
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'meta
      mac-use-title-bar nil)

(use-package general)
(general-create-definer leader-def
                        ;; :prefix my-leader
                        :prefix "SPC")

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

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
  (require 'nano-layout)
  (require 'nano-defaults)
  (require 'nano-session))

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :custom-face
  (nano-mono ((t (:family "Input Mono" :height 240))))
  (nano-italic ((t (:family "Input Mono" :height 240 :slant italic))))
  (nano-sans ((t (:family "Lato" :height 240))))
  :config
  (setq nano-fonts-use t)
  (nano-dark))

(use-package nano-splash
  :custom
  (nano-splash-duration 20)
  :straight (nano-splash :type git :host github :repo "gaoDean/nano-splash")
  :config (nano-splash))

(use-package nano-modeline
  :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
  :config (nano-modeline-mode))

(setq default-frame-alist '((min-height . 1)  '(height . 45)
                          (min-width  . 1)  '(width  . 81)
                          (vertical-scroll-bars . nil)
                          (internal-border-width . 24)
                          (undecorated . t)
                          (tool-bar-lines . 0)
                          (menu-bar-lines . 1)))

(setq frame-resize-pixelwise t)

(use-package mixed-pitch
  :hook text-mode)

(use-package all-the-icons)

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
  :custom
  (avy-keys '(?i ?s ?r ?t ?g ?p ?n ?e ?a ?o))
  :general
  (leader-def '(normal visual) "j" 'avy-goto-char-2)
  (general-def "C-j" 'avy-goto-char-2))

(use-package helpful
  :general
  (leader-def 'normal
    "h F" 'describe-face
    "h p" 'describe-package
    "h f" 'helpful-callable
    "h v" 'helpful-variable
    "h k" 'helpful-key
    "h x" 'helpful-command
    "h ." 'helpful-at-point))

(setq ido-ignore-buffers '("^ " "\*"))

(use-package magit
  :general
  (leader-def 'normal "g g" 'magit))

(use-package projectile
    :config
    (projectile-mode)
    :general
    (leader-def 'normal
        "p p" 'projectile-switch-project
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
  :init
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
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
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection)

(use-package shrink-path)
(use-package eshell-vterm
  :config
  (defalias 'eshell/v 'eshell-exec-visual))
(use-package eshell-up)

(use-package dirvish
    :straight (dirvish :type git :host github :repo "isamert/dirvish")
    :custom
    (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
     '(("h" "~/"                          "Home")
       ("d" "~/Downloads/"                "Downloads")
       ("v" "~/vau/"                      "vau")
       ("r" "~/repos/"                    "repos")
       ("t" "~/.Trash"                    "Trash")))
    :init
    (dirvish-override-dired-mode)
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
            "w c" 'evil-window-close
            "w v" 'evil-window-vsplit
            "w >" 'evil-window-increase-width
            "w <" 'evil-window-decrease-width
            "w +" 'evil-window-increase-height
            "w -" 'evil-window-decrease-height

            ;; file
            "f r" 'recentf

            ;; emacs
            "e r" 'dg/reload-init-file
            "r" 'jump-to-register)

(general-define-key :states '(normal visual)
            "g j" 'evil-next-visual-line
            "g j" 'evil-previous-visual-line
            "C-u" 'evil-scroll-up)

(general-define-key "C-v" 'evil-paste-after)

(general-define-key (kbd "C-x C-m") 'execute-extended-command)
