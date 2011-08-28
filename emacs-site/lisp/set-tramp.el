;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; The only transfer modes that are extremely fast for file access
;; (from Linux) are 'sshx' and 'plink'.
;;
;; The plink binary is not as commonly installed as openssh is,
;; but just as good for tramp.
;;
;; I need to figure out a way for SSH_ASKPASS to work on Windows,
;; and also for handling ssh-add TTY when ASKPASS is not possible
;; (ie, console).
;;

(require 'tramp)

(defun tramp-open-connection-ssh-agent (multi-method method user host)
  "A wrapper for the tramp open connection rsh method.
The wrapper first calls ssh-agent when necessary to acquire
the users key."
  (shell-command "ssh-add -l || ssh-add")
  (tramp-open-connection-rsh multi-method method user host)
  )
(defun my-tramp-setup-w32 ()
  "tramp settings for Windows"
  (setq tramp-default-method "plink")
  ;; (setenv "SSH_ASKPASS" "")
  )

(defun my-tramp-setup-nix ()
  "tramp settings for *NIX"
  (setq tramp-default-method "sshx")
  )
(defun my-tramp-setup-x11-agent ()
  (setenv "SSH_ASKPASS" "x11-ssh-askpass")
  ;; I don't know how to edit all list entries containing 'ssh'
  ;; to use my connection function instead of the original.
  ;; So instead, I just add another method, identical to 'sshx' method,
  ;; calling my own connection function.
  (add-to-list 'tramp-methods
               '("sshxagent"
                 (tramp-connection-function tramp-open-connection-ssh-agent)
                 (tramp-login-program "ssh")
                 (tramp-copy-program nil)
                 (tramp-remote-sh "/bin/sh")
                 (tramp-login-args
                  ("-e" "none" "-t" "-t" "/bin/sh"))
                 (tramp-copy-args nil)
                 (tramp-copy-keep-date-arg nil)
                 (tramp-password-end-of-line nil)) )
  (setq tramp-default-method "sshxagent")
  )

(defun my-tramp-setup-x11 ()
  "tramp settings for X11 on UNIX with openssh installed"
  (cond ((locate-file "ssh-agent" exec-path) (my-tramp-setup-x11-agent))
        (t (my-tramp-setup-nix)) ))

(defun my-tramp-setup ()
  (cond
    ((eq window-system 'w32) (my-tramp-setup-w32))
    ((eq window-system 'x) (my-tramp-setup-x11))
    ((getenv "DISPLAY") (my-tramp-setup-x11))
    (t (my-tramp-setup-nix)) ))

;; setup
(my-tramp-setup)

;; The old vc-svn does not support remote files correctly.
;; However, it appears to work with 22.0.91
(defadvice vc-svn-registered (around my-vc-svn-registered-tramp activate)
  "Disable vc-svn operations for file access via TRAMP."
  (if (and (< emacs-major-version 22)
           (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p (ad-get-arg 0)))
      nil
    ad-do-it))

(provide 'set-tramp)
