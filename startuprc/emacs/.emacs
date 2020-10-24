;; Dark theme
(load-theme 'misterioso)

;; save temporary files at ~/.emacs.d/backup/
(setq backup-directory-alist `(("." . "~/.emacs.d/backup/"))
	  auto-save-file-name-transforms `((".*" "~/.emacs.d/backup/" t)))

(setq org-todo-keywords (quote ((sequence "TODO(t!)" "NEXT(n!)" "STARTED(S)" "|" "DONE(d!/!)" "COMPLETED(l!)" "VERIFIED(v)")
                                (sequence "FIXED(F@/!)" "WAITING(w@/!)" "PROGRESS(p)" "SCHEDULED(h@)" "POSTPONED(P@/!)" "|" "CANCELLED(c@/!)" "FAILED(f!)")
                                (sequence "SUCCEEDED(s)" "DEFERRED(D)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                                (sequence "OPEN(O!)" "PENDING(N@/!)" "WORKAROUND(W@/!)" "|" "NONISSUE(I)" "NOTREPRODUCIBLE(r)" "CLOSED(C!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("STARTED"    :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("COMPLETED"      :foreground "forest green" :weight bold)
              ("VERIFIED"  :foreground "forest green" :weight bold)
              ("FIXED"   :foreground "green"       :weight bold)
              ("WAITING"   :foreground "yellow"       :weight bold)
              ("PROGRESS"  :foreground "blue"         :weight bold)
              ("SCHEDULED"      :foreground "blue"         :weight bold)
              ("POSTPONED"      :foreground "orange"         :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold)
	      ("FAILED"    :foreground "orangered"    :weight bold)
              ("SUCCEEDED" :foreground "hotpink"      :weight bold)
              ("DEFERRED"  :foreground "indianred1"   :weight bold)
              ("APPROVED"  :foreground "forest green" :weight bold)
              ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
              ("REJECTED"  :foreground "olivedrab"    :weight bold)
              ("OPEN"      :foreground "red"      :weight bold)
              ("PENDING"   :foreground "magenta"      :weight bold)
              ("WORKAROUND" :foreground "orangered"      :weight bold)
              ("NONISSUE"  :foreground "forest green"      :weight bold)
              ("NOTREPRODUCIBLE"      :foreground "forest green"      :weight bold)
	      ("CLOSED"    :foreground "forest green" :weight bold))))


;; Hide stars and indent levels with 1, 3, 5, ... stars
(setq org-hide-leading-stars t)    
(setq org-odd-levels-only t)

;;  Avoid the annoying startup message.
(setq inhibit-startup-message t)

;; Setup a nice theme if we have the color-theme package loaded.
(if (require 'color-theme nil t)
    (if window-system
     	(color-theme-arjen)
      (color-theme-arjen)))

;; Global fontlock
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; Show the time on the status bar.
(setq display-time-24hr-format t)
(display-time)

;;
;; Make all "yes or no" prompts show "y or n" instead
;;
(fset 'yes-or-no-p 'y-or-n-p)

;; Never show the toolbar
(tool-bar-mode 0)

;; Enable Ido-mode always
(ido-mode t)


;; Kill current buffer without confirmation, unless its modified.
(global-set-key "\C-xk" 'kill-this-buffer)


;; visual-line word wrap
(global-visual-line-mode t)


;; Load .el scripts from ~/elisp
(add-to-list 'load-path "~/elisp")

;; Emacs as a tiling window manager a.l.a xMonad 
;; (require 'emacsd-tile)

;; Emacs as a tiling window manager a.l.a xMonad 
;; (require 'emacsd-tile)

;; display images inline
(setq org-startup-with-inline-images t)


;; Use org-babel to highlight syntax in code blocks
(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("31deed4ac5d0b65dc051a1da3611ef52411490b2b6e7c2058c13c7190f7e199b" default)))
 '(org-export-backends (quote (ascii html icalendar latex md freemind))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "❂" "◌" "◎"  "⚪" "◦"))


;; (add-to-list 'custom-theme-load-path
;;         (concat
;;          "~/elisp/"
;;          (elt (directory-files "~/elisp/" nil "org-beautify-theme-[0-9].*") 0)))
;;(add-hook 'org-mode-hook (lambda () (load-theme 'org-beautify)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (C . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)         ;this is the important one for this tutorial
   (ocaml . nil)
   (octave . t)
   (python . t)
   (ruby . t)
   (screen . nil)
   (shell . t)
   (sql . nil)
   (sqlite . t)))


;; Use down arrow instead of ellipsis
(setq org-ellipsis " ▾"
       org-hide-emphasis-markers t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-ellipsis ((t (:foreground "LightGoldenrod" :underline nil)))))


(setq-default
 tab-width 4
 indent-tabs-mode t)

;; show parentheses-match
(show-paren-mode 1)

;; Match parantheses on '%'
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
    "Go to the matching paren if on a paren; otherwise insert %."
	(interactive "p")
	(cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
		  ((looking-at "\\s)") (forward-char 1) (backward-list 1))
		  (t (self-insert-command (or arg 1)))))


(setq c-default-style "linux"
      c-basic-offset 4)


