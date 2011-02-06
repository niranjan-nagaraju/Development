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

(setq org-todo-keywords (quote ((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)")
                                (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                                (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "yellow"       :weight bold)
              ("SOMEDAY"   :foreground "goldenrod"    :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold)
              ("QUOTE"     :foreground "hotpink"      :weight bold)
              ("QUOTED"    :foreground "indianred1"   :weight bold)
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

;; Always show the toolbar
(tool-bar-mode)