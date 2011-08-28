;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; configure `server' for local and remote on Multi-TTY emacs

(require 'server)
(setq server-use-tcp t)
(setq server-host
      (if (executable-find "/sbin/ip")
          
          (let ((i (shell-command-to-string "ip route get 1")))
            (string-match " src \\([0-9.]+\\) " i)
            (match-string 1 i))
        
        nil))

; Suppress error "directory ~/.emacs.d/server is unsafe" on Windows 2000
(when (and (= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (setq server-host "127.0.0.1"))

(provide 'set-server)
