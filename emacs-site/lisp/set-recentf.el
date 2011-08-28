;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'recentf)

(setq recentf-auto-cleanup 'never)
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items 500)

(recentf-mode 1)

;; FIXME - abbrev tramp files to 'host-:-basename', or something like that
;; a fancier, and more legible, version, with concise filenames
; FIXME -- I don't want to use CL for copy-list

(defun effecting-file-name-history-alist (historylist)
  "Generate an alist of strings: (cons \"abbreviation\" \"filename\") ."
    (require 'cl)
    
    (let* ((hist (copy-list historylist))
           (al (mapcar (lambda (x)
                         (let ((y (file-name-nondirectory x))
                               (z (string-match "^[A-Za-z]+://" x)))
                           (cons (cond ((and (> (length y) 0) (null z)) y)
                                       (t x))
                                 x)))
                       (delete-dups hist)))
           (cal (mapcar 'car al)))
      
      ; for duplicate items, replace abbrev. with full path
      (dolist (v al)
        (if (member (car v) (cdr (member (car v) cal)))
            (setcar v (cdr v))
            ))

      al
      ))

(require 'iswitchb)
;; recentf with iswitchb style minibuffer interface because menus are bad

(defun effecting-recentf-find-file (fname)
  "Using iswitchb, interactively chose a recent file to call `find-file' with.
The choices are full file paths."
  (interactive (list (let ((iswitchb-make-buflist-hook (lambda () (setq iswitchb-temp-buflist recentf-list))))
                       (iswitchb-read-buffer "recent file "))))
  (find-file fname))

; file-name-history

(defun effecting-recentf-find-file-concise (fname)
  "Using iswitchb, interactively chose a recent file to call `find-file' with.
The choices are abbreviated file paths. Only the filename is shown if it is unique.
Otherwise, the fewest number of directories to make the abbreviation unique show shown."
  (interactive (list (let* ((al (effecting-file-name-history-alist recentf-list))
                            (iswitchb-max-to-show 50)
                            (iswitchb-make-buflist-hook (lambda ()
                                                          (setq iswitchb-temp-buflist (mapcar 'car al))
                                                          )))
                       (cdr (assoc (iswitchb-read-buffer "recent file ") al))
                       )))
  (find-file fname))

(defun effecting-recentf-view-file-concise (fname)
  "Using iswitchb, interactively chose a recent file to call `view-file' with.
The choices are abbreviated file paths. Only the filename is shown if it is unique.
Otherwise, the fewest number of directories to make the abbreviation unique show shown."
  (interactive (list (let* ((al (effecting-file-name-history-alist recentf-list))
                            (iswitchb-max-to-show 50)
                            (iswitchb-make-buflist-hook (lambda ()
                                                          (setq iswitchb-temp-buflist (mapcar 'car al))
                                                          )))
                       (cdr (assoc (iswitchb-read-buffer "recent file ") al))
                       )))
  (view-file fname))

(provide 'set-recentf)
