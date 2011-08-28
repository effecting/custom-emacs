;; 
;; 

;; Authored by effecting2 in 2007.
;;
;; wv-mode.el is a Major Mode for viewing wvText output in color.
;;
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information. 

;; This code is not part of GNU Emacs.

;; Commentary:
;;     wvText invokes wvWare to convert a Word document to HTML;
;;     it then uses elinks, links, and/or lynx to dump.
;;
;;     This major mode provides simple font-lock for the output of
;;     wvText, making tables and headings somewhat more appeasing.

;; POTENTIAL TODO
;;

(defvar wv-mode-font-lock-keywords
  '(("[+|][-+|]*[+|]" 0 font-lock-constant-face)
    ("|" 0 font-lock-constant-face)
    ("^[ \t\r]*\n[ \t\r]*\\([0-9]+[.].*\n?.*\\)\n[ \t\r]*$" 1 font-lock-variable-name-face)
    ("^[ \t\r]*\n[ \t\r]*[*] " 0 font-lock-keyword-face))
  "Font-lock highlighting for `wv-mode'.")

(define-derived-mode wv-mode text-mode "wv"
  "Major mode for viewing wvText output."
  :group 'wv-mode
  (set (make-local-variable 'tab-width) 8)
  (if (boundp 'font-lock-defaults)
      (set (make-local-variable 'font-lock-defaults)
	   '(wv-mode-font-lock-keywords nil t))))
