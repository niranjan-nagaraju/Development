
(setq org-todo-keywords (quote ((sequence "TODO(t!)" "NEXT(n!)" "STARTED(S)" "|" "DONE(d!/!)" "COMPLETED(l!)" "VERIFIED(v)")
                                (sequence "FIXED(F@/!)" "WAITING(w@/!)" "PROGRESS(p)" "|" "CANCELLED(c@/!)" "FAILED(f!)")
                                (sequence "SUCCEEDED(s)" "DEFERRED(D)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                                (sequence "OPEN(O!)" "PENDING(P@/!)" "WORKAROUND(W@/!)" "|" "NONISSUE(N)" "NOTREPRODUCIBLE(r)" "CLOSED(C!)"))))

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

;; Always show the toolbar
(tool-bar-mode)

;; Enable Ido-mode always
(ido-mode t)


;; Kill current buffer without confirmation, unless its modified.
(global-set-key "\C-xk" 'kill-this-buffer)