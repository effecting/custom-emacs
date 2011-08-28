;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'eudcb-ldap)

;; configure EUDC for local LDAP lookup
;; (setq mail-directory-function 'my-mail-get-names)
(setq eudc-server "localhost")
(setq ldap-host-parameters-alist '(("localhost" base "ou=people,dc=effecting,dc=us" auth simple sizelimit 10)))
(setq eudc-inline-expansion-format '("%s <%s>" displayName mail))
(setq eudc-inline-query-format '( (name) (cn) (rdn) (uid) (mail) (displayName) (sn) (firstname) ))
(setq eudc-query-function 'eudc-ldap-simple-query-internal)

(defun effecting-mail-ldap-dump ()
   "Perform LDAP lookup on substring and return list of potential completions.
Return value is list of (name . address)."
  (delete-dups (mapcar (lambda (x)
                         (cons (cdr (nth 1 x))
                               (cdr (nth 0 x))))
                       (eudc-query (list (cons "mail" "*")) '(displayName mail))
                       )))

(defun effecting-mail-ldap-query (str)
  "Perform LDAP lookup on substring and return list of potential completions.
Return value is list of (name . address)."
  (let* ((pat (concat "*" str "*"))
         (que (list (cons "cn" pat)
                    (cons "rdn" pat)
                    (cons "uid" pat)
                    (cons "mail" pat)
                    (cons "displayName" pat)) )
         (get '(displayName mail))
         (ret '())
        )
    (while que
      (let* ((a (list (car que)))
              (b (eudc-query a get)))
        (setq ret (append ret (mapcar (lambda (x) (cons (cdr (nth 1 x)) (cdr (nth 0 x)))) b)))
        (setq que (cdr que)) ))
    (delete-dups ret)
    ))

(defun effecting-mail-expand-ldap ()
  "Expand the query substring before point from LDAP database."
  (interactive)
  (let* ((end (point))
         (begin (mh-beginning-of-word))
         (input (buffer-substring-no-properties begin end))
         (cal (mapcar (lambda (x) (concat "\"" (cdr x) "\" <" (car x) ">"))
                      (effecting-mail-ldap-query input)))
         (len (length cal))
         (iswitchb-make-buflist-hook (lambda () (setq iswitchb-temp-buflist cal)))
         (val (cond ((< len 1) nil)
                    ((= len 1) (car cal))
                    ((> len 1) (iswitchb-read-buffer "addr" nil)))))
    (unless val
      (message "Failed to expand: %s" input))
    (when val
      (delete-region begin end)
      (insert val)
      )))
  
(provide 'set-eudc)
