;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'shell)

;; pick a minimal bash-like shell as available,
;; and make this setting specific to 'shell'
(defadvice shell (around my-shell activate)
  (let ((explicit-shell-file-name
         (or (executable-find "dash")
             (executable-find "sash")
             (executable-find "bash")
             (executable-find "sh")
             explicit-shell-file-name)))
    ad-do-it))

;; readline's M-_ binding
(define-key shell-mode-map (kbd "M-_")
  '(lambda ()
     (interactive)
     (insert (car (last (split-string (ring-ref comint-input-ring 0))))) ))

(when custom-dir
  (setenv "ENV" (expand-file-name (concat custom-dir "/shinit"))) )     

(provide 'set-shell)
