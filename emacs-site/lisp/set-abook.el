;; 
;; 

;; Authored by effecting2 in 2011.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'abook)

(defun effecting-mail-abook-query (str)
  "Perform vCard address book lookup on substring and return list of
potential completions.
Return value is list of (address . name)."
  (abook-open)
  (let (card-index attr-index card search-cards ret attr names emails email name)
    (setq ret '())
    (setq search-cards (abook-search-cards str (list "fn" "email")))
    (dolist (card-index search-cards)
      (setq card (abook-get-card card-index))
      (setq names '())
      (setq emails '())
      (dotimes (attr-index (vcard-get-num-attributes card))
        (setq attr (vcard-get-attribute card attr-index))
        (if (member (vcard-attr-get-name attr) (list "fn"))
            (setq names (append names (vcard-attr-get-values attr))))
        (if (member (vcard-attr-get-name attr) (list "email"))
            (setq emails (append emails (vcard-attr-get-values attr))))
        )
      (dolist (email emails)
        (dolist (name names)
          (if (or (string-match str email)
                  (string-match str name))
              (setq ret (append ret (list (cons email name))))
            )))
      )
    (delete-dups ret)
  ))

(defun effecting-mail-expand-abook ()
  "Expand the query substring before point from address book."
  (interactive)
  (let* ((end (point))
         (begin (mh-beginning-of-word))
         (input (buffer-substring-no-properties begin end))
         (cal (mapcar (lambda (x) (concat "\"" (cdr x) "\" <" (car x) ">"))
                      (effecting-mail-abook-query input)))
         (len (length cal))
         (iswitchb-make-buflist-hook (lambda () (setq iswitchb-temp-buflist cal)))
         (val (cond ((< len 1) nil)
                    ((= len 1) (car cal))
                    ((> len 1) (iswitchb-read-buffer "addr" nil))))
         )
    (unless val
      (message "Failed to expand: %s" input))
    (when val
      (delete-region begin end)
      (insert val)
      )
    ))
  
(provide 'set-abook)
