;; 
;; 

;; Authored by effecting2 in 2008.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; Based upon example at URL: http://dfa.slackware.it/NopasteFromEmacs.html
;; This code is not part of GNU Emacs.

(defun pastebin-region-to-file (start end)
  "Save current region in a temporarily file and return the filename as string"
  (let ((filename (make-temp-file "pastebin")))
    (kill-ring-save start end)
    (find-file filename)
    (yank)
    (save-buffer)
    (kill-buffer (substring filename 5))
    filename))

(defun pastebin-send (file)
  "Send the file to pastebin (using wgetpaste), the open the default browser
    to the resulting page"
  (browse-url
    (substring
    (shell-command-to-string (concat "wgetpaste" " " file)) 0 -1)))

(defun pastebin-region (start end)
  "Send the current region to pastebin"
  (interactive "r")
  (pastebin-send (pastebin-region-to-file start end)))

(defun pastebin-buffer ()
  "Send the current buffer to pastebin"
  (interactive)
  (pastebin-send (buffer-file-name)))
