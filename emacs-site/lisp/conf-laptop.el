;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; settings for laptops

(setq battery-mode-line-format " [%p%% %t]")
(display-battery-mode t)
(display-time-mode t)

(defun effecting-alsa-mute (&optional arg)
  (interactive "P")
  (let ((mute (cond ((and arg (< arg 1)) "unmute")
                    (t "mute"))))
    (message "ALSA: Master: %s" mute)
    (call-process "amixer" nil nil nil "set" "Master" mute)
    ))
(global-set-key (kbd "<s-left>") 'effecting-alsa-mute)

(provide 'conf-laptop)
