;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; enable terminal mouse mode via GPM

;;(when (functionp 't-mouse-mode)
;;   (t-mouse-mode +1))

(require 'xt-mouse)

;; for Xterm
(defun mouse-scroll-up (&optional x)
  (interactive "P")
  (if (equal x nil)
      (scroll-up '5)
    (scroll-up x))
  )

(defun mouse-scroll-down (&optional x)
  (interactive "P")
  (if (equal x nil)
      (scroll-down '5)
    (scroll-down x))
  )

;; Xterm mouse mode settings
(setq x-select-enable-clipboard t)
(when (functionp 'xterm-mouse-mode)
  (global-set-key (kbd "<mouse-4>") 'mouse-scroll-down)
  (global-set-key (kbd "<mouse-5>") 'mouse-scroll-up)
  )

(provide 'set-xt-mouse)
