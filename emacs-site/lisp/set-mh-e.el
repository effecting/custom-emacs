;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'mh-loaddefs)
(require 'mh-e)
(require 'mh-mime)

(mh-find-path) ; init MH correct without running mh-rmail

; other settings needed
(require 'set-abook)

mh-lpr-command-format

(require 'set-lpr)
(when effecting-lpr-command
  (setq mh-lpr-command-format effecting-lpr-command))

(require 'iswitchb)

;; both nmh and Mailutils are installed, select one
;; over time, I've learned the hard way that nmh loves to segfault,
;; but also supports more functions (mu-mh is missing flists, ...)
(when (not (boundp 'my-mh))
  (setq my-mh 'nmh)
  )
(when (eq my-mh 'nmh)
  (setq mh-variant "nmh 1.3")
  (setq mh-path '("/usr/bin/mh" ))
  )
(when (eq my-mh 'gnu)
  (setq mh-variant "GNU Mailutils 1.2")
  (setq mh-path '("/usr/bin/mu-mh"))
  )

;; various settings
(setq mh-letter-fill-column 72)
(setq mh-compose-insertion 'mml)
(setq mh-x-face-file "~/.facejunksdf3539573958w3ythswekltghasdlh")
(setq mh-large-folder nil)
(setq mh-compose-prompt-flag nil)
(setq mh-yank-behavior t)
(setq mh-compose-forward-as-mime-flag t)
(setq mh-invisible-header-fields '("X-Real"
                                   "X-IronPort"
                                   "X-Cephire"
                                   "X-TFF"))
(setq mh-search-program 'mairix)
(setq mh-large-folder 1000)

;; configure MH-E to use abook lookups instead of MH alias files
(setq mh-alias-expand-aliases-flag t)
(setq mh-alias-flash-on-comma 1)
(setq mh-alias-insert-file nil)
(setq mh-alias-local-users nil)
(setq mh-letter-complete-function-alist
      '((bcc . effecting-mail-expand-abook)
        (cc . effecting-mail-expand-abook)
        (dcc . effecting-mail-expand-abook)
        (fcc . mh-folder-expand-at-point)
        (from . effecting-mail-expand-abook)
        (mail-followup-to . effecting-mail-expand-abook)
        (mail-reply-to . effecting-mail-expand-abook)
        (reply-to . effecting-mail-expand-abook)
        (to . effecting-mail-expand-abook))
      )


;; folder management
(setq mh-ticked-messages-folders nil)

;; some identities and folders stored in custom file, not here, for privacy
;; autoloads

;; Advice to handle mailboxes the way I want
(defun effecting-mh-identity-auto (err)
  (if mh-current-folder
      (let* ((f mh-current-folder)
             (m (string-match "[+]\\(.*\\)" f))
             (n (replace-match "\\1" t nil f))
             (i (assoc n mh-identity-list)))
        (if i
            (setq mh-identity-default n)
          (setq mh-identity-default my-mh-identity-failsafe)))
    (when err
      (message "Failed to select default identity after visiting folder."))))

(defadvice mh-visit-folder (after my-mh-visit-folder activate)
  (effecting-mh-identity-auto t))

;; default Fcc:
(defun my-mh-default-folder-for-message (&optional id)
  (let ((i (cond ((assoc id mh-identity-list) id)
                 (mh-identity-default)) ))
    (concat "+" i "_sent")))
(setq mh-default-folder-for-message-function 'my-mh-default-folder-for-message)

(defadvice mh-insert-identity (around my-mh-insert-identity activate)
  "Advise `mh-insert-identity' to also change the `Fcc:' field to reflect the
inserted identity. Also handle auto signing."
  ;;(mh-mml-unsecure-message)
  (setq ad-return-value ad-do-it)
  (let* ((args (ad-get-args 0))
         (mh-identity-default (car args)))
    (mh-header-field-delete "Fcc" t)
    (mh-to-fcc (my-mh-default-folder-for-message (concat "+" identity)))
    (mh-goto-header-end 2))
  )

(defadvice mh-reply (around effecting-mh-reply activate)
  "Advise `mh-reply' to change the default identity according to the original
folder from which the email came, should the message come from an index/sequence buffer."
  (let* ((tim-seq (string-match "[+]mhe-index/sequence/" mh-current-folder))
         (tim-dir (cond (tim-seq (save-excursion
                                    (let ((done nil)
                                          (skip nil))
                                      (move-to-column 0)
                                      (setq done (looking-at "^\\([+][A-Za-z0-9]+\\)"))
                                      (setq skip (< (point) 2))
                                      (while (and (not done) (not skip))
                                        (setq done (looking-at "^\\([+][A-Za-z0-9]+\\)"))
                                        (setq skip (< (point) 2))
                                        (forward-line -1))
                                      (if done
                                          (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                        nil))))
                        (t nil))))
                     
    (when (and tim-seq tim-dir)
      (let ((mh-current-folder tim-dir))
        (effecting-mh-identity-auto nil)))
    (setq ad-return-value ad-do-it)
    ))


;;;; MH-E MIME options
(require 'set-mm)
;; media tests - appending would be faster but not idempotent
(mapc (lambda (x) (add-to-list 'mh-mm-inline-media-tests x))
      effecting-mm-inline-media-tests)

(setq mh-mml-method-default (if mh-pgp-support-flag
                                "pgpmime"
                              "smime"))

;; key bindings

;; debugging
; (defadvice mh-exec-cmd (after my-mh-exec-cmd activate)
;  (message (ad-get-args 0)))

;; (defun effecting-mh-show-longlines ()
;;   "Hook function for enabling longlines minor mode on all mh-show buffers."
;;   )
;; (add-hook 'mh-show-hook #'effecting-mh-show-longlines))

(defun effecting-mail-source-new-mail-p ()
  "Handler for `display-time' to indicate when new mail is available."
  (> (length (shell-command-to-string
                  (apply 'concat "flists" " -noshowzero"
                         (mapcar (lambda (x) (concat " +" x)) mh-new-messages-folders))))

     10))
(setq display-time-mail-function #'effecting-mail-source-new-mail-p)

(defun effecting-change-mh-mml-method-default (&optional method)
  "Change MH-E method between pgpmime and smime."
  (interactive (list (let ((iswitchb-make-buflist-hook (lambda () (setq iswitchb-temp-buflist '("pgpmime" "smime")))))
                       (iswitchb-read-buffer "MH-E security method "))))
  (setq mh-mml-method-default method)
  (message "MH-E security method changed to: %s" method)
  )

(defun effecting-change-mh-mml-method-default-pgpmime ()
  (interactive)
  (effecting-change-mh-mml-method-default "pgpmime"))

(defun effecting-change-mh-mml-method-default-smime ()
  (interactive)
  (effecting-change-mh-mml-method-default "smime"))

(defadvice mh-letter-mode (after effecting-mh-letter-mode activate)
  "Add some key strokes."
  (local-set-key (kbd "C-c RET 1") 'effecting-change-mh-mml-method-default-pgpmime)
  (local-set-key (kbd "C-c RET 2") 'effecting-change-mh-mml-method-default-smime))


; S/MIME identity handling for EPG
(defvar mh-identity-mml-smime-signers nil
  "Holds the S/MIME key ID to be used by mml-smime.el.
This is normally set as part of an Identity in
`mh-identity-list'.")
(make-variable-buffer-local 'mh-identity-mml-smime-signers)

(defun effecting-mh-identity-handler-smime-identity (field action &optional value)
  "Process header FIELD \":mml-smime-signers\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added.
The global variable `mh-identity-mml-smime-signers' is set to
VALUE when action 'add is selected."
  (cond
   ((or (equal action 'remove)
        (not value)
        (string= value ""))
    (setq mh-identity-mml-smime-signers nil))
   ((equal action 'add)
    (setq mh-identity-mml-smime-signers (list value))))
  )

(defadvice mh-mml-to-mime (around effecting-mh-mml-to-mime activate)
    "Advise `mh-mml-to-mime' let some global values"
    (let ((mml-smime-signers mh-identity-mml-smime-signers))
      (message-options-set 'mml-smime-epg-signers nil)
      (setq ad-return-value ad-do-it)))

(add-to-list 'mh-identity-handlers
             '(":mml-smime-signers" . effecting-mh-identity-handler-smime-identity) t)

;; auto sign
(defun effecting-mh-identity-handler-auto-sign (field action &optional value)
  "Process header FIELD \":auto-mml-sign\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added.
The global variable `mh-identity-mml-smime-signers' is set to
VALUE when action 'add is selected."
  (cond
   ((or (equal action 'remove)
        (not value)
        (string= value ""))
    (mh-mml-unsecure-message))
   ((equal action 'add)
    (mh-mml-secure-message-sign value)))
  )

(add-to-list 'mh-identity-handlers
             '(":auto-mml-sign" . effecting-mh-identity-handler-auto-sign) t)

(provide 'set-mh-e)
