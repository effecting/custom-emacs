;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'pgg)

;; settings
(setq pgg-query-keyserver t)
(setq pgg-default-scheme 'gpg)
(setq pgg-encrypt-for-me t)
(setq pgg-gpg-use-agent t)
(setq pgg-gpg-extra-args nil)
(setq pgg-ignore-packet-checksum t)
; (setq pgg-passphrase-cache-expiry 300)
(setq pgg-default-user-id "contact@effecting.us")
    
;; use EPA/EPG if available
(when (and (locate-library "pgg-epg")
           (locate-library "epa"))
  (require 'epa)
  (require 'pgg-epg)
  (setq pgg-default-scheme 'epg)
  )

(provide 'set-pgg)
