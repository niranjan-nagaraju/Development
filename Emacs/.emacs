(setq journal-file "~/journal.org")

(defun start-journal-entry ()
  "Start a new journal entry."
  (interactive)
  (find-file journal-file)
  (goto-line 4)
  (org-insert-heading)
  (org-insert-time-stamp (current-time) t)
  (open-line 2)
  (insert " "))

(global-set-key (kbd "C-c j") 'start-journal-entry)

(setq org-todo-keywords (quote ((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!/!)" "SCHEDULED(S@/!)")
                                (sequence "STARTED(T)" "COMPLETED(l)" "WAITING(w@/!)" "PROGRESS(p)" "|" "CANCELLED(c@/!)" "FAILED(f!)")
                                (sequence "SUCCEEDED(s)" "DEFERRED(D)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                                (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("SCHEDULED" :foreground "blue"         :weight bold)
              ("STARTED"   :foreground "green"        :weight bold)
	      ("COMPLETED"   :foreground "blue"        :weight bold)
              ("WAITING"   :foreground "yellow"       :weight bold)
              ("PROGRESS"  :foreground "blue"         :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold)
	      ("FAILED"    :foreground "orangered"    :weight bold)
              ("SUCCEEDED" :foreground "hotpink"      :weight bold)
              ("DEFERRED"  :foreground "indianred1"   :weight bold)
              ("APPROVED"  :foreground "forest green" :weight bold)
              ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
              ("REJECTED"  :foreground "olivedrab"    :weight bold)
              ("OPEN"      :foreground "magenta"      :weight bold)
              ("CLOSED"    :foreground "forest green" :weight bold))))

;;Cycle the buffers
(global-set-key [(control tab)] 'bs-cycle-next)

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

;; Ido mode on
(ido-mode t)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-level-3 ((t (:inherit outline-6))))
 '(org-level-4 ((t (:inherit outline-3))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(org-level-6 ((t (:inherit outline-4)))))

;; Kill current buffer without confirmation, unless its modified
(global-set-key "\C-xk" 'kill-this-buffer)


;; visua-line mode wrap
(global-visual-line-mode t)
