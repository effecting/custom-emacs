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

;;; rxvt-unicode
(mapc (lambda (x)
        (define-key function-key-map (car x) (cdr x)))
      '(("\e[1~" . [home])
        ("\e[4~" . [end])
        ("\e[5~" . [prior])
        ("\e[6~" . [next])
        ("\eOa" . [C-up])
        ("\eOb" . [C-down])
        ("\eOc" . [C-right])
        ("\eOd" . [C-left])
        ("\eOm" . [kp-subtract])
        ("\eOj" . [kp-multiply])
        ("\eOo" . [kp-divide])
        ("\eOM" . [kp-enter])))

;;; gnome-terminal
(mapc (lambda (x)
        (define-key function-key-map (car x) (cdr x)))
      '(("\eO3A" . [M-up])
        ("\eO3B" . [M-down])
        ("\eO3C" . [M-right])
        ("\eO3D" . [M-left])
        ("\eO5A" . [C-up])
        ("\eO5B" . [C-down])
        ("\eO5C" . [C-right])
        ("\eO5D" . [C-left])
        ("\eO7A" . [C-M-up])
        ("\eO7B" . [C-M-down])
        ("\eO7C" . [C-M-right])
        ("\eO7D" . [C-M-left])
        ("\e[Z" . [S-iso-lefttab])))

(provide 'conf-ui-terminal)
