;; 
;; 

;; Authored by effecting2 in 2010.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(setq initial-scratch-message nil)     ; no scratch message
;(setq initial-buffer-choice t)         ; visit scratch
(setq inhibit-startup-echo-area-message
      (if (equal init-file-user "")
          (user-login-name)
        init-file-user))               ; inhibit startup messages

(provide 'preset-startup)
