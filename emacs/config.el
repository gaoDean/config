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

;; (straight-use-package 'use-package) ;; emacs 29 built-in use-package
(require 'use-package)
(setq straight-use-package-by-default t)

(setq straight-host-usernames
      '((github . "gaoDean")
        (gitlab . "gaoDean")))

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :custom-face
  (default ((t (:family "Input Mono" :height 260))))
  (italic ((t (:family "ETBembo" :slant italic))))
  (variable-pitch ((t (:family "ETBembo"))))
  ;; :config
  ;; (nano-dark)
  ;; (nano-light)
  )

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (nano-light))
    ('dark (nano-dark))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(use-package nano-splash
  :custom
  (nano-splash-duration 20)
  :straight (nano-splash :type git :host github :repo "gaoDean/nano-splash")
  :config (nano-splash))

(defun mode-line-render (left right)
  ;; (let* ((available-width (- (window-width) (length left) )))
  (let* ((available-width (- (frame-width) (length left) )))
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
                                         (propertize "[M]" 'face `(:inherit nano-faded)))))
                  (format-mode-line (propertize "%4l:%2c  " 'face `(:inherit nano-faded)))))))
(setq-default mode-line-format nil)
(set-face-attribute 'header-line nil
                    :underline nil
                    :background 'unspecified)

(set-face-background 'header-line nil)
(advice-add 'nano-light :after (lambda(&rest r) (set-face-background 'header-line nil)))
(advice-add 'nano-dark :after (lambda(&rest r) (set-face-background 'header-line nil)))

;; (setq display-buffer-base-action
;;       '((display-buffer-in-side-window)
;;         (side . right)
;;         (window-width . 0.5)))

(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

(use-package esup)

(use-package general)
(general-create-definer leader-def
                        ;; :prefix my-leader
                        :prefix "SPC")
(general-evil-setup t)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer t
        evil-undo-system 'undo-fu)
  :config
  (setq ring-bell-function 'ignore)
  (setq evil-insert-state-message nil)
  (setq evil-visual-char-message nil)
  (setq evil-visual-line-message nil)
  (setq evil-visual-block-message nil)
  (evil-mode 1))

(use-package evil-collection
  :hook (emacs-startup . evil-collection-init))

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

(setq user-full-name "Dean Gao"
      user-mail-address "gao.dean@hotmail.com")

(add-to-list 'load-path "~/.config/emacs/plugins/")

(use-package mixed-pitch
  :config
  (setq mixed-pitch-fixed-pitch-faces (remove 'org-table mixed-pitch-fixed-pitch-faces))
  :hook org-mode)

(use-package valign
  :hook org-mode
  :init
  (setq valign-fancy-bar t)
  (advice-add 'evil-toggle-fold :after (lambda(&rest r) (valign-mode)(valign-mode)))
  :config
  (set-face-attribute 'org-table nil :family "ETBembo" :foreground "FFF"))

(add-hook 'after-init-hook (lambda() (use-package all-the-icons)))

(use-package writeroom-mode
    :commands writeroom-mode
    :hook org-mode
    :custom
    (writeroom-fullscreen-effect 'maximized)
    (writeroom-fringes-outside-margins nil)
    (writeroom-width (* 0.55 (- 1 0.125)))
    (writeroom-added-width-left (lambda() (round (* (window-width) 0.125))))
    (writeroom-header-line '((:eval
                     (mode-line-render
                      (format-mode-line (list
                                         (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                                                     'help-echo "Mode(s) menu"
                                                     'mouse-face 'mode-line-highlight
                                                     'local-map   mode-line-major-mode-keymap)
                                         " %b "
                                         (if (and buffer-file-name (buffer-modified-p))
                                             (propertize "[M]" 'face `(:inherit nano-faded)))))
                      ""))))
)

(setq-default fill-column 81 ;; Thou shalt not cross 80 columns in thy file
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

;; Save miscellaneous history
(setq savehist-additional-variables
      '(kill-ring
        command-history
    set-variable-value-history
    custom-variable-history   
    query-replace-history     
    read-expression-history   
    minibuffer-history        
    read-char-history         
    face-name-history         
    bookmark-history          
        ivy-history               
    counsel-M-x-history       
    file-name-history         
        counsel-minibuffer-history))
(setq history-length 250)
(setq kill-ring-max 25)
(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)
(put 'ivy-history                'history-length 25)
(put 'counsel-M-x-history        'history-length 25)
(put 'counsel-minibuffer-history 'history-length 25)
(setq savehist-file "~/.cache/emacs/savehist")
(savehist-mode 1)

;; Remove text properties for kill ring entries
;; See https://emacs.stackexchange.com/questions/4187
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

;; Recentf files 
(setq recentf-max-menu-items 25)
(setq recentf-save-file     "~/.cache/emacs/recentf")
(add-hook 'emacs-startup-hook #'recentf-mode)

;; Bookmarks
(setq bookmark-default-file "~/.cache/emacs/bookmarks")

;; Backup
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups"))
      make-backup-files t     ; backup of a file the first time it is saved.
      backup-by-copying t     ; don't clobber symlinks
      version-control t       ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 6     ; oldest versions to keep when a new numbered
                              ;  backup is made (default: 2)
      kept-new-versions 9     ; newest versions to keep when a new numbered
                              ;  backup is made (default: 2)
      auto-save-default t     ; auto-save every buffer that visits a file
      auto-save-timeout 20    ; number of seconds idle time before auto-save
                              ;  (default: 30)
      auto-save-interval 200)  ; number of keystrokes between auto-saves
                              ;  (default: 300)

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
  (setq which-key-idle-delay 1.5)
  :hook emacs-startup
  :config
  (which-key-setup-side-window-right))

(setq ido-ignore-buffers '("^ " "\*"))

(use-package hl-todo
  :hook (emacs-startup . global-hl-todo-mode))

(setq auto-save-file-name-transforms
    '((".*" "~/.cache/emacs/autosave" t)))

(use-package undo-fu)
(use-package undo-fu-session
  :custom
  (undo-fu-session-directory "~/.cache/emacs/undo-fu-session")
  :hook (emacs-startup . undo-fu-session-global-mode))

(defun my/set-face (face inherit)
  (face-spec-reset-face face)
  (set-face-attribute face nil :inherit inherit))

(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode))

(use-package whitespace
  :straight nil
  :commands whitespace-mode
  :init
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-line-column 81) ;; Thou shalt not cross 80 columns in thy file
  :config
  (my/set-face 'whitespace-line 'nano-critical)
  (my/set-face 'whitespace-trailing 'nano-critical-i)
  (my/set-face 'whitespace-tab 'nano-critical-i))

(defun sis-mode ()
  (interactive)
  (sis-context-mode t)
  (sis-inline-mode t))

(use-package sis
  :commands sis-mode
  :config
  (setq sis-other-cursor-color 'orange)
  (sis-ism-lazyman-config
   "com.apple.keylayout.Australian"
   "com.sogou.inputmethod.sogou.pinyin")
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)
  )

(electric-pair-mode 1)

(add-hook 'mhtml-mode-hook (lambda()
                             (setq sgml-quick-keys 'close)
                             (setq electric-pair-inhibit-predicate
                                   `(lambda (c)
                                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
                             ))
(setq electric-pair-pairs
    '(
      (?\" . ?\")
      (?\' . ?\')
      (?\{ . ?\})
      (?\( . ?\))
      (?\[ . ?\])))

(advice-add 'org-edit-table.el :before (lambda (&rest r) (set-face-attribute 'table-cell nil :background 'unspecified)))

(use-package emacs-pa
  :straight (emacs-pa :type git :host github :repo "gaoDean/emacs-pa" :local-repo "~/repos/rea/emacs-pa")
  :general
  (leader-def :keymaps 'normal
    "P" '(:ignore t :wk "pa")
    "P a" 'pa--add
    "P e" 'pa--edit
    "P s" 'pa--show
    "P d" 'pa--delete
    "P r" 'pa--rename))

(add-to-list 'image-file-name-extensions "avif")
  (add-to-list 'image-types 'avif)

(setq org-html-inline-image-rules '(("file" . "\\(?:\\.\\(?:gif\\|jp\\(?:e?g\\)\\|png\\|avif\\|svg\\|webp\\)\\)")
                                   ("http" . "\\(?:\\.\\(?:gif\\|jp\\(?:e?g\\)\\|png\\|avif\\|svg\\|webp\\)\\)")
                                   ("https" . "\\(?:\\.\\(?:gif\\|jp\\(?:e?g\\)\\|png\\|avif\\|svg\\|webp\\)\\)")))

(use-package simpleclip
  :hook emacs-startup-hook
  :general
  (general-def general-override-mode-map
    "M-v" 'simpleclip-paste
    "M-c" 'simpleclip-copy))

(use-package org :straight (:type built-in))
(advice-add 'org-open-at-point :after #'delete-other-windows)

(defun my/set-org-faces()
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-document-title nil :height 2.2 :foreground "FFF")
    (set-face-attribute 'org-level-1 nil :font "ETBembo" :weight 'normal :height 1.5)
    (set-face-attribute 'org-level-2 nil :font "ETBembo" :weight 'normal :slant 'italic :height 1.35)
    (set-face-attribute 'org-level-3 nil :font "ETBembo" :weight 'bold :height 1.1)
    (set-face-attribute 'org-modern-symbol nil :font "FiraCode NF" :height 1.1)))

(advice-add 'nano-light :after (lambda(&rest r) (my/set-org-faces)))
(advice-add 'nano-dark :after (lambda(&rest r) (my/set-org-faces)))

(use-package org-modern
  :init
  (setq
   org-catch-invisible-edits 'show-and-error
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
   org-modern-label-border 0.3
   ;; org-modern-hide-stars ""
   ;; org-modern-hide-stars " "
   org-modern-hide-stars t
   org-modern-table nil ;; TODO theres a bug
   org-image-actual-width '(400)
   line-spacing 0.1
   org-pretty-entities t
 org-modern-block-name
      '(("src" "»" "«")
        ("quote" "❝" "❞")
        ("export" "⇄")
          (t . t)
        )
      org-modern-keyword
      '((t . t)
        ("title" . ""))
   org-ellipsis "…"
   ;; org-ellipsis ":"
   )
  :hook org-mode
  :config
  (set-face-attribute 'org-modern-block-name nil :family "ETBembo")
  (my/set-org-faces))

(use-package org-autolist :hook org-mode)

(use-package org-fragtog
  :init
  (setq org-startup-with-latex-preview nil
        org-latex-create-formula-image-program 'dvisvgm
        org-highlight-latex-and-related '(latex script entities)
        org-latex-preview-ltxpng-directory "~/.cache/emacs/ltxpng/")
  :config
  (plist-put org-format-latex-options :scale 2.6)
  :hook org-mode)

(use-package org-appear
  :hook org-mode
  :custom
  (org-appear-autoentities t)
  (org-appear-submarkers t)
  (org-appear-autolinks t))

(use-package org-auto-tangle :hook org-mode)

(add-hook 'org-mode-hook
          (lambda()
            (require 'ox-latex)
            (setq org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "qlmanage -p %s"))))
            (setq org-latex-default-class "org")

            (setq
             ;; org-latex-pdf-process (list "latexmk -f -pdfxe -interaction=nonstopmode -output-directory=%o %f")
             org-latex-pdf-process (list "xelatex -interaction nonstopmode -output-directory=%o %f"
                                         "xelatex -interaction nonstopmode -output-directory=%o %f")
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
                    ("" "amsmath" t)
                    ("" "amssymb" t)
                    ("" "capt-of" nil)
                    ("" "hyperref" t))
                  org-latex-packages-alist
                  '(
                    ("" "hyphenat")
                    ("" "listings")
                    ("" "float")
                    ("" "placeins")
                    ))


            (add-to-list 'org-latex-classes
                         '("org"
                           "
               \\documentclass[hidelinks]{article}
               \\def\\UrlBreaks{\\do\\/\\do-\\do_} 
               [DEFAULT-PACKAGES]
               [PACKAGES]
               [EXTRA]"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(use-package citeproc :if (eq major-mode 'org-mode))

(use-package htmlize :if (eq major-mode 'org-mode))

    (setq org-html-postamble-format '(("en" "
    <p class=\"author\">Author: %a</p>
    <p class=\"updated\">Updated: %C</p>
    <p class=\"creator\">%c</p>
    ")))

    ;; preserve line breaks 
    ;; example: don't join line1 and line2 together into a paragraph
    (setq org-export-preserve-breaks t)

 (setq org-html-head-include-default-style nil)

    (setq org-publish-timestamp-directory "~/.cache/emacs/org-timestamps/")  
    (setq org-publish-project-alist
          '(("org-notes"
             :base-directory "~/org/"
             :publishing-directory "~/org/pub/"
             :base-extension "org"
             :publishing-function org-html-publish-to-html
             :exclude "pub"
             :recursive t
             :html-extension "html"
             :auto-preamble nil
             :html-preamble nil
             :html-postamble nil
             :head-include-default-style nil
             :section-numbers nil
             :with-toc nil
;;              :html-head "
;; <link rel=\"stylesheet\" href=\"/web/main.css\" type=\"text/css\"/>
;; <link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\">
;; <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css\">
;; "
             :html-head "
<link rel=\"stylesheet\" href=\"/web/main.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"/web/tufte.min.css\"/>
"
             )
            ("org-static"
             :base-directory "~/org/"
             :base-extension "css\\|avif\\|js\\|json\\|xlsx\\|docx\\|png\\|webp\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html"
             :publishing-directory "~/org/pub/"
             :exclude "pub"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("org" :components ("org-notes" "org-static"))))

(use-package org-imgtog
  :custom
  (org-imgtog-preview-delay 0.2)
  :straight (org-imgtog :type git :host github :repo "gaoDean/org-imgtog" :local-repo "~/repos/rea/org-imgtog")
  :hook org-mode)

(setq url-cache-directory "~/.cache/emacs/url")
(setq org-display-remote-inline-images 'cache)
;; (setq org-display-remote-inline-images 'download)

(use-package org-remoteimg
  :straight (org-remoteimg :type git :host github :repo "gaoDean/org-remoteimg" :local-repo "~/repos/rea/org-remoteimg")
  :after org-imgtog)

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

(use-package tempel
  :config
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (setq tempel-path "~/.config/emacs/templates.el")
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  (auto-insert-mode)
  (setq auto-insert-query nil)
  (define-auto-insert "\\.org$" [(lambda() (evil-insert 0) (tempel-insert 'tempel-org))])
  (define-auto-insert "\\.html" [(lambda() (evil-insert 0) (tempel-insert 'tempel-html))])
  (define-auto-insert "\\.svelte" [(lambda() (evil-insert 0) (tempel-insert 'tempel-svelte))])
  )

(use-package tempel-collection :after tempel)

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

(use-package cape
  :config

  (defun cape-setup-capf ()
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    ;; (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    ;; (add-to-list 'completion-at-point-functions #'cape-history)
    ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
    ;; (add-to-list 'completion-at-point-functions #'cape-tex)
    ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
    ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;; (add-to-list 'completion-at-point-functions #'cape-dict)
    ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
    ;; (add-to-list 'completion-at-point-functions #'cape-line)
    )

  (add-hook 'prog-mode-hook 'cape-setup-capf)
  (add-hook 'text-mode-hook 'cape-setup-capf)

  )

(set-register ?c (cons 'file "~/.config/emacs/config.org"))
(set-register ?i (cons 'file "~/org/index.org"))
(set-register ?t (cons 'file "~/.config/emacs/templates.el"))

(defun my/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun my/copy-buffer ()
  "copy the entire buffer to clipboard"
  (interactive)
  (kill-new (buffer-string)))

(defun my/org-open-at-point ()
  (interactive)
  (org-open-at-point))

(defun my/run-code ()
  "run code based on current language"
  (interactive)
  (let* ((interpreters '((js . "node")
                         (py . "python")))
         (inter (alist-get (intern (file-name-extension (buffer-file-name))) interpreters) ))
    (shell-command (concat inter " " (buffer-file-name)))))

(defun my/run-code-input (input)
  "run code based on current language with stdin"
  (interactive (list (read-from-minibuffer "Input: ")))
  (let* ((interpreters '((js . "node")
                         (py . "python")))
         (inter (alist-get (intern (file-name-extension (buffer-file-name))) interpreters) ))
    (shell-command (concat "echo -n \"" input "\" | " inter " " (buffer-file-name)))))

(defun my/view-with-quicklook ()
  (interactive)
  (let ((name (buffer-file-name)))
    (if name
        (async-start-process "quicklook" "qlmanage" nil "-p" (concat (file-name-sans-extension name) ".pdf"))
      (message "File not found"))))

(defun my/git-push-org ()
  (interactive)
  (org-publish "org")
  (start-process "git-push-org" nil "gaa" "org")
  (message "done"))

(defun my/git-push-org-force-republish ()
  (interactive)
  (org-publish "org" t)
  (start-process "git-push-org" nil "gaa" "org")
  (message "done"))

(defun my/light-theme ()
  (interactive)
  (nano-light)
  (my/set-org-faces))

(defun my/dark-theme ()
  (interactive)
  (nano-dark)
  (my/set-org-faces))

(defun my/org-cycle ()
  "Cycle through visibility states, but do not reach the SUBTREE state."
  (interactive)
  (if (or (eq (car (org-element-context)) 'plain-list)
          (eq (car (org-element-context)) 'item))
      (org-cycle)
    (evil-toggle-fold)))

(defun my/new-org-file (file)
  "Cycle through visibility states, but do not reach the SUBTREE state."
  (interactive (list (read-from-minibuffer "File: ")))
  (find-file (concat "~/org/" file)))

(unbind-key "s-p") ;; ns-print-buffer

(leader-def :keymaps '(normal visual)
  "b" '(:ignore t :wk "buffers")
  "b b" 'ido-switch-buffer
  "b B" 'ibuffer
  "b K" 'ido-kill-buffer
  ;; "b k" 'kill-this-buffer
  "b k" 'kill-current-buffer
  "b n" 'bs-cycle-next
  "b p" 'bs-cycle-previous

  "w" '(:ignore t :wk "windows")
  "w m" 'delete-other-windows
  "w w" 'evil-window-next
  "w k" 'evil-window-up
  "w j" 'evil-window-down
  "w h" 'evil-window-left
  "w l" 'evil-window-right
  "w p" 'evil-window-mru
  "w c" 'evil-window-delete
  "w v" 'evil-window-vsplit
  "w <" 'evil-window-increase-width
  "w >" 'evil-window-decrease-width
  "w +" 'evil-window-increase-height
  "w -" 'evil-window-decrease-height
  "w n" 'make-frame
  "w d" 'delete-frame

  "o" '(:ignore t :wk "open")
  "o e" 'eshell
  "o v" 'vterm

  "t" '(:ignore t :wk "toggle")
  "t s" 'sis-mode
  "t z" 'writeroom-mode
  "t w" 'whitespace-mode

  "a" '(:ignore t :wk "actions")

  "f" '(:ignore t :wk "files")
  "f r" 'recentf
  "f v" 'my/view-with-quicklook
  "f R" 'rename-file
  "f o" 'my/new-org-file

  "e" '(:ignore t :wk "emacs")
  "e +" 'text-scale-increase
  "e +" 'text-scale-decrease
  "e r" 'my/reload-init-file
  "e m" 'toggle-frame-maximized

  "e e" '(:ignore t :wk "eval")
  "e e r" 'eval-region
  "e e b" 'eval-buffer
  "e e f" 'eval-defun

  "e b" '(:ignore t :wk "benchmark")
  "e b t" 'benchmark-init/show-durations-tabulated
  "e b r" 'benchmark-init/show-durations-tree
  "e b i" 'emacs-init-time

  "e t" '(:ignore t :wk "theme")
  "e t l" 'my/light-theme
  "e t d" 'my/dark-theme

  "E" '(:ignore t :wk "export")
  "E e" 'org-export-dispatch
  "E p" 'org-publish
  "E o" 'my/git-push-org
  "E O" 'my/git-push-org-force-republish

  "c" '(:ignore t :wk "code")
  "c w" 'fill-paragraph
  "c c" 'count-words
  "c r" 'my/run-code
  "c R" 'my/run-code-input
  "c y" 'my/copy-buffer
  "c m" 'make

  "." 'find-file
  "/" 'rg
  "q" 'save-buffers-kill-terminal
  "s" 'scratch-buffer
  "x" 'delete-frame
  "r" 'jump-to-register)

(imap "DEL" 'backward-delete-char-untabify)

(nmap general-override-mode-map 
  :predicate '(derived-mode-p 'org-mode)
  "<return>" 'my/org-open-at-point
  "TAB" 'my/org-cycle
  "<tab>" 'my/org-cycle)

(nmap general-override-mode-map 
  "Y" (general-simulate-key "y$" :state 'normal))

(general-def '(normal visual) general-override-mode-map
  "g j" 'evil-next-visual-line
  "g k" 'evil-previous-visual-line
  "C-u" 'evil-scroll-up)

(imap "M-SPC" (general-simulate-key "SPC" :state 'normal))

(general-def pdf-mode-map
  "SPC" (general-simulate-key "SPC" :state 'normal)
  "q" 'kill-this-buffer)

;; (general-def dired-mode-map
;;   "SPC" (general-simulate-key "SPC" :state 'normal)
;;   "q" 'my/kill-all-dired-buffers-and-quit)

;; (general-define-key "M-v" 'evil-paste-after)

(general-define-key (kbd "C-x C-m") 'execute-extended-command)
(nmap general-override-mode-map "C-n" 'make-frame)

(general-def tempel-map "<tab>" 'tempel-next)
(general-def tempel-map "<backtab>" 'tempel-previous)
