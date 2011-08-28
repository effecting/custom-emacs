;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'ledger)

;; partial clearing isn't too helpful for me
(setq ledger-clear-whole-entries t)

;; use my own ledger script to fix cache
(setq ledger-binary-path (executable-find "ledger.sh"))

;; some reports
(setq ledger-reports (list
                       '("bal" "ledger.sh -s bal")
                       '("reg" "ledger.sh reg")
                       '("bal real" (concat ledger-binary-path " --real -B -s bal"))
                       '("reg real" (concat ledger-binary-path " --real -B reg"))
                       '("expenses" (concat ledger-binary-path " -r reg ^expense"))
                       (list "all" (concat
                                    "echo EXPENSES; "
                                    ledger-binary-path
                                    " -r reg ^expense; "
                                    "echo; "
                                    "echo BALANCES; "
                                    ledger-binary-path
                                    " -s bal; "
                                    "echo; "
                                    ))
                       ))

;; use my own indentation
(setq ledger-enforce-indentation t)

(defun ledger ()
  "A quick shortcut to `find-file' the default ledger file"
  (interactive)
  (let ( (l (getenv "LEDGER_FILE")) )
    (if l
        (find-file l)
      (error "Environment variable LEDGER_FILE is not defined"))
    ))

(provide 'set-ledger)
