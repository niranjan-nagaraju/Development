

(setq org-todo-keywords (quote ((sequence "TODO(t!)" "NEXT(n!)" "STARTED(S)" "|" "DONE(d!/!)" "COMPLETED(l!)" "VERIFIED(v)")
                                (sequence "WAITING(w@/!)" "PROGRESS(p)" "|" "CANCELLED(c@/!)" "FAILED(f!)")
                                (sequence "SUCCEEDED(s)" "DEFERRED(D)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                                (sequence "OPEN(O!)" "PENDING(P@/!)" "WORKAROUND(W@/!)" "|" "NONISSUE(N)" "NOTREPRODUCIBLE(r)" "CLOSED(C!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("STARTED"    :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("COMPLETED"      :foreground "forest green" :weight bold)
              ("VERIFIED"  :foreground "forest green" :weight bold)
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


;; Org-mode export to Google Calendar

;;; define categories that should be excluded
(setq org-export-exclude-category (list "google" "private"))

;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.

(defun org-mycal-export-limit ()
  "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\\)>")
  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (save-excursion
    ; get categories
    (setq mycategory (org-get-category))
    ; get start and end of tree
    (org-back-to-heading t)
    (setq mystart    (point))
    (org-end-of-subtree)
    (setq myend      (point))
    (goto-char mystart)
    ; search for timerange
    (setq myresult (re-search-forward org-tstr-regexp myend t))
    ; search for categories to exclude
    (setq mycatp (member mycategory org-export-exclude-category))
    ; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))

;;; activate filter and call export function
(defun org-mycal-export () 
  (let ((org-icalendar-verify-function 'org-mycal-export-limit))
    (org-export-icalendar-combine-agenda-files)))