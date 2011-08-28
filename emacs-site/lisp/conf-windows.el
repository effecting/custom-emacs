;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; Advise 'other-window' to remember the previously selected window.
;; Then, by calling other-window-toggle, two windows can be rapidly switched
;; between with a single key sequence, definied below.

(defadvice other-window (before other-window-ad-before activate)
  (setq toggle-window-prev (selected-window))
  )

;; Emacs default key for other-window
;; (global-set-key (kbd "C-x o") 'other-window)


;; bindings for other-window in reverse
(defun other-window-reverse (&optional x)
  (interactive "P")
  (if (equal x nil)
      (other-window -1)
    (other-window (- 0 x)) ))
(global-set-key (kbd "C-x M-o") 'other-window-reverse)
(global-set-key (kbd "C-c M-o") 'other-window-reverse)

;; bindings for return to previously selected window
;; as remembered by `other-window-ad-before'
(defun other-window-toggle (&optional x)
  (interactive "P")
  (if (not (boundp 'toggle-window-prev))
      (if (equal x nil)
	  (other-window 1)
	(other-window x))
    (let ( (y toggle-window-prev) )
      (setq toggle-window-prev (selected-window))
      (if (equal toggle-window-prev y)
	  (if (equal x nil)
	      (other-window 1)
	    (other-window x))
	(select-window y))
      ))
  )
(global-set-key (kbd "C-x C-o") 'other-window-toggle)
(global-set-key (kbd "C-c C-o") 'other-window-toggle)

;; convenience windowing operations
(defun buffer-make-writable (&optional x)
  (interactive "P")
  (call-process "chmod" nil nil nil "+w" (buffer-file-name))
  (toggle-read-only)
  )
  
(global-set-key (kbd "C-= n") 'rename-buffer)
(global-set-key (kbd "C-= w") 'buffer-make-writable)

;; Binds Ctrl-PageUP and Ctrl-PageDown to scroll a terminal window
;; since <S-Prior> does not seem to work with most term emulators.
(global-set-key (kbd "M-[ 5 ^") 'scroll-down)
(global-set-key (kbd "M-[ 6 ^") 'scroll-up)
(global-set-key (kbd "<C-prior>") 'scroll-down)
(global-set-key (kbd "<C-next>") 'scroll-up)

(provide 'conf-windows)
