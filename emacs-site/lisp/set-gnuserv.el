;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; configure gnuserv for local and via tramp
;; assumes ~/.ssh/config configured with:
;;    RemoteForward 9191 localhost:9191

(require 'gnuserv-compat)

(when (boundp 'custom-dir)
  (setenv "GNU_SECURE" (expand-file-name (concat custom-dir "/gnu_secure"))) )

;; do not create new frames
(setq gnuserv-frame t)

(defadvice gnuserv-start (before my-setup-gnuclient activate)
  (setenv "GNU_HOST" "127.0.0.1")
  (setenv "GNU_PORT" "9191")
  (mapc (lambda (x) (setenv x "gnuclient"))
        '("VISUAL" "EDITOR" "SVN_EDITOR") )
  )

;; one key to save and close
(defun gnuserv-edit-save (&optional COUNT)
  (interactive)
  (save-buffer)
  (gnuserv-edit COUNT)
  )
(global-set-key (kbd "C-x C-3") 'gnuserv-edit-save)
(global-set-key (kbd "C-c C-3") 'gnuserv-edit-save)

(provide 'set-gnuserv)

