;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; Requires "xmlunicode.el" from:
;;     http://nwalsh.com/emacs/xmlchars/

(autoload 'unicode-character-insert "xmlunicode" nil t)
(autoload 'unicode-character-shortcut-insert "xmlunicode" "" t)
(autoload 'iso8879-character-insert "xmlunicode" nil t)

(global-set-key (kbd "C-x 9") 'unicode-character-shortcut-insert)
(global-set-key (kbd "C-x 7") 'iso8879-character-insert)
(global-set-key (kbd "C-c 9") 'unicode-character-insert)
(global-set-key (kbd "C-c 7") 'smart-chars-mode)

(define-minor-mode smart-chars-mode
  "Toggle SmartChars unicode punctuation mode." nil " SmartChars"
  '(("\'" . unicode-smart-single-quote)
    ("\"" . unicode-smart-double-quote)
    ("\;" . unicode-smart-semicolon)
    ("\-" . unicode-smart-hyphen)
    ("\." . unicode-smart-period))
  (if (not (commandp 'unicode-smart-period)) (load "xmlunicode")))
