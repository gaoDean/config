(setq
 site-run-file nil                         ; No site-wide run-time initializations. 
 inhibit-default-init t                    ; No site-wide default library
 gc-cons-threshold most-positive-fixnum    ; Very large threshold for garbage
 package-enable-at-startup nil)            ; We'll use straight.el

(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache" user-emacs-directory)))

;; Reset garbage collector limit after init process has ended (8Mo)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

(setq default-frame-alist '((min-height . 1)  '(height . 45)
                            (min-width  . 1)  '(width  . 81)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 24)
                            (undecorated . t)
                            (tool-bar-lines . 0)
                            (fullscreen . maximized)
                            (menu-bar-lines . 0)))

(setq initial-frame-alist default-frame-alist)

;; https://stackoverflow.com/questions/27758800/why-does-emacs-leave-a-gap-when-trying-to-maximize-the-frame
(setq frame-resize-pixelwise t)

(setq use-short-answers t)
(setq mac-command-modifier 'meta
      mac-option-modifier 'meta)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-compacting-font-caches t)

(setq initial-scratch-message nil)
(setq initial-buffer-choice nil)
(setq frame-title-format nil)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq pop-up-windows nil)
(setq indicate-empty-lines nil)
(setq cursor-in-non-selected-windows nil)
(setq initial-major-mode 'fundamental-mode)
(setq default-major-mode 'fundamental-mode)
(setq font-lock-maximum-decoration nil)
(setq font-lock-maximum-size nil)
(setq auto-fill-mode nil)
(setq fill-column 81) ;; Thou shalt not cross 80 columns in thy file
(setq confirm-nonexistent-file-or-buffer nil)
(setq org-return-follows-link t)
(setq backward-delete-char-untabify-method 'hungry)
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups")))

;; https://web.archive.org/web/20170413150436/https://ogbe.net/emacsconfig.html
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (display-graphic-p)
    (menu-bar-mode t) ;; When nil, focus problem on OSX
  (menu-bar-mode -1))
(pixel-scroll-precision-mode 1)
(pixel-scroll-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)
(setq window-min-height 1)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")
(setq-default shell-file-name "/bin/zsh")
(setq explicit-shell-file-name "/bin/zsh")

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun display-startup-echo-area-message ()
  (message ""))
