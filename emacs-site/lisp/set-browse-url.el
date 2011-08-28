;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

(require 'browse-url)

(setq browse-url-firefox-program "firefox")
(setq browse-url-firefox-arguments nil)
(setq browse-url-generic-program "xopen")
(setq browse-url-generic-arguments nil)

(setq browse-url-browser-function
      (cond
       ((eq window-system 'x)
        (cond
         ((executable-find browse-url-generic-program)   'browse-url-generic)
         ((executable-find browse-url-firefox-program)   'browse-url-firefox)
         ((executable-find browse-url-seamonkey-program) 'browse-url-seamonkey)
         ((executable-find browse-url-mozilla-program)   'browse-url-mozilla)
         (t                                              'w3m-browse-url)))
       ;; ((eq window-system 'w32)
       ;;  browse-url-default-windows-browser)
       ((eq window-system 'w32)
        browse-url-firefox-program)
       ((eq window-system 'mac) 
        browse-url-default-macosx-browser)
       (t 'w3m-browse-url)
       ))

(provide 'set-browse-url)
