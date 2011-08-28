;; 
;; 

;; Authored by effecting2 in 2009.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'erc)
(require 'erc-networks)
(require 'erc-services)
(require 'erc-log)

(erc-services-mode 1)
(setq erc-log-p t)
(setq erc-nickserv-identify-mode 'both)
(setq erc-prompt-for-nickserv-password t)

(defadvice erc-nickserv-identify-autodetect (around my-erc-auto activate)
  (erc-log "erc-nickserv-identify-autodetect advice outter proc")
  (let* ((proc (ad-get-arg 0))
         (parsed (ad-get-arg 1)))

    (erc-log "erc-nickserv-identify-autodetect advice inner proc")

;;     (unless (and (null erc-nickserv-passwords)
;;                  (null erc-prompt-for-nickserv-password))
      (let* (
             (network (erc-network))
             (sender (erc-nickserv-alist-sender network))
             (identify-regex (erc-nickserv-alist-regexp network))
             (sspec (erc-response.sender parsed))
             (nick (car (erc-response.command-args parsed)))
             (msg (erc-response.contents parsed))
             )
        
        (erc-log (concat "ZZZ network: " (symbol-name network) " ZZZ"))
        (erc-log (concat "ZZZ sender: " sender " ZZZ"))
        (erc-log (concat "ZZZ identify-regex: " identify-regex " ZZZ"))
        (erc-log (concat "ZZZ sspec: " sspec " ZZZ"))
        (erc-log (concat "ZZZ nick: " nick " ZZZ"))
        (erc-log (concat "ZZZ msg: " msg " ZZZ"))
        
        )
;;       )
    )
  ad-do-it)

(provide 'debug-erc)
