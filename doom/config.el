(setq user-full-name "Dean Gao"
      user-mail-address "gao.dean@hotmail.com")

(beacon-mode 1)

(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "Input" :size 20)
      doom-variable-pitch-font (font-spec :family "Open Sans" :size 20)
      doom-big-font (font-spec :family "Input" :size 26))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(set-face-attribute 'mode-line nil :font doom-font)
(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(after! org
  (setq org-directory "~/org/"
        org-ellipsis " ▼ "
        org-log-done 'time
        org-hide-emphasis-markers t
        org-table-convert-region-max-lines 20000))

(custom-set-faces!
  `(org-superstar-header-bullet :font "FiraCode NF" :height 0.99 :weight light))

(defun dg/set-org-header-size ()
  (interactive)
  (dolist
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
  (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf")

(after! org
  (dg/set-org-header-size))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun dt/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'dt/insert-auto-tangle-tag)

(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook '+zen/toggle)

(map! :after org :map org-mode-map :nvi "S-<return>" #'+org/insert-item-below)
(map! :after org :map org-mode-map :nvi "<return>" #'+org/insert-item-below)
