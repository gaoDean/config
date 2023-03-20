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
  `(org-superstar-header-bullet :font "FiraCode NF" :height 0.99 :weight light))

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
  (setq org-auto-tangle-default t))

(defun dg/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'dg/insert-auto-tangle-tag)

(add-hook 'org-mode-hook 'org-autolist-mode)

(setq math-preview-command "/Users/deangao/.local/share/npm/bin/math-preview")

(defalias #'org-latex-preview #'math-preview-at-point)
(defalias #'org-clear-latex-preview #'math-preview-clear-region)

(beacon-mode 1)

;; (setq fancy-splash-image "~/.config/doom/black-hole.png")

(setf dired-kill-when-opening-new-dired-buffer t)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file"           "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(set-face-attribute 'mode-line nil :font "Input-16")
(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(after! rainbow-mode (global-rainbow-mode 1))

(defun stop-using-minibuffer ()
    "kill the minibuffer"
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-fragtog-mode)
;; (add-hook 'org-mode-hook '+zen/toggle)


