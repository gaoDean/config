(setq user-full-name "Dean Gao"
      user-mail-address "gao.dean@hotmail.com")

;; (require 'benchmark-init)
;; (add-hook 'doom-first-input-hook #'benchmark-init/deactivate)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2023/bin/universal-darwin"))
(setq exec-path (append exec-path '("/usr/local/texlive/2023/bin/universal-darwin")))

(setq doom-theme 'doom-moonlight)
;; (setq doom-theme 'gruber-darker)

;; (custom-set-faces!
;;   `(default :background "#282c34"))

(add-to-list 'default-frame-alist '(undecorated . t))

;; (set-face-background 'default "mac:windowBackgroundColor")
;; (set-face-stipple 'default "alpha:80%")

(pixel-scroll-precision-mode)

(setq doom-font (font-spec :family "Input" :size 20)
      doom-variable-pitch-font (font-spec :family "Merriweather" :size 20)
      doom-big-font (font-spec :family "Input" :size 26))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default nil))

(defun dg/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'dg/insert-auto-tangle-tag)

(add-hook 'org-mode-hook 'org-autolist-mode)

(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-appear-autolinks t
      org-appear-autosubmarkers t
      org-appear-autoentities t
      org-appear-autokeywords t
      org-appear-inside-latex t
      org-appear-delay 0.1)

(setq org-startup-with-latex-preview t)
(setq org-latex-create-formula-image-program 'dvisvgm)
(after! org
  (plist-put org-format-latex-options :scale 2.6))

(after! org
  (setq org-highlight-latex-and-related '(latex script entities)))

(after! org-fragtog (org-fragtog-table-mode))

(setq ob-mermaid-cli-path "/Users/deangao/.local/share/npm/bin/mmdc")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (latex . t)
   (emacs-lisp . t)))
(setq org-babel-default-header-args:mermaid
      '((:results . "file")
        (:exports . "results")
        (:puppeteer-config-file . "/Users/deangao/.config/puppeteer/config.json")))

(after! org
  (setq org-directory "~/des/"
        org-ellipsis " ▼ "
        org-pretty-entities t
        org-superstar-item-bullet-alist '((?- . ?➤) (?+ . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        org-table-convert-region-max-lines 20000)
(display-line-numbers-mode nil))

(custom-set-faces!
  `(org-superstar-header-bullet :font "FiraCode NF" :height 1.1 :weight light)
  `(org-ellipsis :height 0.8 :underline nil :foreground "#484848"))

(defun dg/set-org-header-size ()
  (interactive)
  (with-eval-after-load 'org-faces (dolist
      (face
       '((org-level-1 1.4 "#51afef" ultra-bold)
         (org-level-2 1.3 "#c678dd" extra-bold)
         (org-level-3 1.2 "#98be65" bold)
         (org-level-4 1.1 "#da8548" semi-bold)
         (org-level-5 1.0 "#5699af" normal)
         (org-level-6 1.0 "#a9a1e1" normal)
         (org-level-7 1.0 "#46d9ff" normal)
         (org-level-8 1.0 "#ff6c6b" normal)))
    (set-face-attribute (nth 0 face) nil :font "Source Sans Pro" :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face))))
  (with-eval-after-load 'org-tables (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf")))

(dg/set-org-header-size)

(after! org
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

(beacon-mode 1)

;; (setq treesit-extra-load-path '("~/.local/share/treesitter"))
;; (require 'treesit)

;; (after! treesit-auto
;;   (global-treesit-auto-mode)
;;   (setq treesit-auto-install 'prompt)

;;   (setq svelte-tsauto-config
;;         (make-treesit-auto-recipe
;;          :lang 'svelte
;;          :ts-mode 'svelte-ts-mode
;;          :remap 'svelte-mode
;;          :url "https://github.com/Himujjal/tree-sitter-svelte"
;;          :revision "master"
;;          :source-dir "src"))

;;   (add-to-list 'treesit-auto-recipe-list svelte-tsauto-config))

(after! eshell (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual))

(setq avy-timeout-seconds 0.2)
;; (map! :leader :desc "Avy jump" "j" #'avy-goto-char-timer)
(map! :leader :desc "Avy jump" "j" #'avy-goto-char-2)

(setq web-mode-script-padding 0)
(setq web-mode-style-padding 0)

(setq fancy-splash-image "~/.config/doom/splash/emacs-big-e-padded.svg")

(after! spell-fu
  (setq spell-fu-idle-delay 0.5))  ; default is 0.25

(use-package dirvish
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
  (kbd "z") 'dired-do-compress)

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

(setq visual-fill-column-width 80
      visual-fill-column-center-text t)

;; (use-package! eaf
;;   :load-path "~/.config/emacs/site-lisp/emacs-application-framework"
;;   :commands (eaf-open)
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
;;   :config
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-all-the-icons)
;;   (require 'eaf-demo)
;;   (require 'eaf-evil)
;;   (setq eaf-pdf-dark-mode nil)

;;   (define-key key-translation-map (kbd "SPC")
;;     (lambda (prompt)
;;       (if (derived-mode-p 'eaf-mode)
;;           (pcase eaf--buffer-app-name
;;             ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
;;                            (kbd "SPC")
;;                          (kbd eaf-evil-leader-key)))
;;             ("pdf-viewer" (kbd eaf-evil-leader-key))
;;             ("image-viewer" (kbd eaf-evil-leader-key))
;;             (_  (kbd "SPC")))
;;         (kbd "SPC")))))

;; (use-package! eaf
;;   :load-path "~/.config/emacs/site-lisp/emacs-application-framework/"
;;   :commands (eaf-open)
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
;;   :config
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-all-the-icons)
;;   (setq eaf-pdf-dark-mode nil)
;;     ;; (require 'eaf-evil)
;;     ;; (define-key key-translation-map (kbd "SPC")
;;     ;;   (lambda (prompt)
;;     ;;     (if (derived-mode-p 'eaf-mode)
;;     ;;         (pcase eaf--buffer-app-name
;;     ;;           ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
;;     ;;                          (kbd "SPC")
;;     ;;                        (kbd eaf-evil-leader-key)))
;;     ;;           ("pdf-viewer" (kbd eaf-evil-leader-key))
;;     ;;           ("image-viewer" (kbd eaf-evil-leader-key))
;;     ;;           (_  (kbd "SPC")))
;;     ;;       (kbd "SPC")))))
;;   )

(set-face-attribute 'mode-line nil :font "Input-16")
(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t  ;; adds folder icon next to persp name
      doom-modeline-enable-word-count t
      doom-modeline-battery t
      doom-modeline-percent-position nil)

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(after! rainbow-mode (global-rainbow-mode 1))

(evil-define-key 'normal pong-mode-map
  (kbd "n") 'pong-move-down
  (kbd "e") 'pong-move-up
  (kbd "t") 'pong-move-right
  (kbd "r") 'pong-move-left)

;; (zone-when-idle 30)

(after! typit (setq typit-test-time 30))

;; (defun stop-using-minibuffer ()
;;     "kill the minibuffer"
;;     (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
;;       (abort-recursive-edit)))

;; (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(add-hook 'org-mode-hook (lambda()
                           (display-line-numbers-mode -1)
                           (text-scale-increase 1)
                           (mixed-pitch-mode)
                           (visual-line-mode)
                           (org-fragtog-mode)
                           (writeroom-mode)))

(evil-define-key 'normal org-mode-map
  (kbd "s-<return>") 'org-meta-return
  (kbd "g j") 'evil-next-visual-line
  (kbd "g k") 'evil-previous-visual-line)

(map! :leader
      :desc "Open small vterm window" "o v" #'vterm
      :desc "Grep" "c g" #'deadgrep)

(after! embark
  (defvar-keymap embark-table-actions
    :doc "table.el functions"
    :parent embark-general-map
    "d" #'table-query-dimension
    "w" #'table-widen-cell
    "n" #'table-narrow-cell)
  (add-to-list 'embark-keymap-alist '(org-table . embark-table-actions)))
