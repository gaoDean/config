(setq user-full-name "Dean Gao"
      user-mail-address "gao.dean@hotmail.com")

(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))

(setq doom-theme 'doom-one)
;; (setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'macchiato)

(defun dg/toggle-dark-mode ()
  (interactive)
  (if (equal catppuccin-flavor 'latte)
      (setq catppuccin-flavor 'macchiato)
    (setq catppuccin-flavor 'latte))
  (catppuccin-reload))

(map! :leader
      :desc "Toggle dark mode" "t d" #'dg/toggle-dark-mode)

(setq whitespace-mode nil)

(setq doom-font (font-spec :family "Input" :size 18)
      doom-variable-pitch-font (font-spec :family "Open Sans" :size 18)
      doom-big-font (font-spec :family "Input" :size 26))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(after! org
  (setq org-directory "~/des/"
        org-ellipsis " ▼ "
        org-pretty-entities t
        org-startup-with-inline-images t
        org-image-actual-width '(300)
        org-log-done 'time
        org-hide-emphasis-markers t
        org-table-convert-region-max-lines 20000))

(custom-set-faces!
  `(org-superstar-header-bullet :font "FiraCode NF" :height 1.1 :weight light))

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

;; (setq math-preview-command "/Users/deangao/.local/share/npm/bin/math-preview")

;; (defalias #'org-latex-preview #'math-preview-at-point)
;; (defalias #'org-clear-latex-preview #'math-preview-clear-region)

(setq org-startup-with-latex-preview t)
(setq org-latex-create-formula-image-program 'dvisvgm)
(after! org (plist-put org-format-latex-options :scale 2.2))

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(beacon-mode 1)

(setq avy-timeout-seconds 0.2)

;; (setq fancy-splash-image "~/.config/doom/black-hole.png")

(after! spell-fu
  (setq spell-fu-idle-delay 0.5))  ; default is 0.25

(add-hook 'after-init-hook 'benchmark-init/deactivate)

(dirvish-override-dired-mode)

(map! :leader
      (:prefix ("d" . "dirvish")
       :desc "Open dirvish" "d" #'dired
       :desc "Dirvish jump to current" "j" #'dired-jump))


(use-package dirvish
    :init
    (dirvish-override-dired-mode)
    :custom
    (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
     '(("h" "~/"                          "Home")
       ("d" "~/Downloads/"                "Downloads")
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
    ;; :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
    ;; (("C-c f" . dirvish-fd)
    ;;  :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
    ;;  ("a"   . dirvish-quick-access)
    ;;  ("f"   . dirvish-file-info-menu)
    ;;  ("y"   . dirvish-yank-menu)
    ;;  ("N"   . dirvish-narrow)
    ;;  ("^"   . dirvish-history-last)
    ;;  ("h"   . dirvish-up-directory)
    ;;  ("l"   . dirvish-open-file)
    ;;  ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
    ;;  ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
    ;;  ("TAB" . dirvish-subtree-toggle)
    ;;  ("M-f" . dirvish-history-go-forward)
    ;;  ("M-b" . dirvish-history-go-backward)
    ;;  ("M-l" . dirvish-ls-switches-menu)
    ;;  ("M-m" . dirvish-mark-menu)
    ;;  ("M-t" . dirvish-layout-toggle)
    ;;  ("M-s" . dirvish-setup-menu)
    ;;  ("M-e" . dirvish-emerge-menu)
    ;;  ("M-j" . dirvish-fd-jump)))

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
  (kbd "R") 'dirvish-renaming-menu
  (kbd "a") 'dirvish-quick-access
  (kbd "c") 'dirvish-chxxx-menu
  (kbd "d") 'dired-do-delete
  (kbd "f") 'dirvish-file-info-menu
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file
  (kbd "m") 'dired-mark
  (kbd "p") 'dirvish-yank
  (kbd "r") 'dired-do-rename
  (kbd "t") 'dired-do-touch
  (kbd "u") 'dired-unmark
  (kbd "v") 'dirvish-move
  (kbd "y") 'dirvish-yank-menu
  (kbd "z") 'dired-do-compress)

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

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

;; (defun stop-using-minibuffer ()
;;     "kill the minibuffer"
;;     (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
;;       (abort-recursive-edit)))

;; (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-fragtog-mode)
;; (add-hook 'org-mode-hook '+zen/toggle)

(map! :leader :desc "Open small vterm window" "o v" #'vterm)
(map! :leader :desc "Avy jump" "j" #'avy-goto-char-timer)
(evil-define-key 'normal org-mode-map
  (kbd "s-<return>") '+org/insert-item-below
  (kbd "g j") 'evil-next-visual-line
  (kbd "g k") 'evil-previous-visual-line)
