;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; flymake for syntax checking

(require 'flymake)
(require 'vhdl-mode)

;; a few settings
(setq flymake-no-changes-timeout 1.0)
(setq flymake-start-syntax-check-on-newline nil)

;;;;;;;;;;;; GHDL syntax checking for `vhdl-mode'
(defun flymake-ghdl-init ()
  "This function implements syntax checking of `vhdl-mode' source with the
GHDL compiler. It inherits some of the `vhdl-mode' options, including
`vhdl-standard'."
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ghdl" (list "-s"
                       (cond ((and (vhdl-standard-p '93) (vhdl-standard-p '87)) "--std=93c")
                             ((vhdl-standard-p '93) "--std=93")
                             ((vhdl-standard-p '87) "--std=87")
                             (t "--std=93c"))
                       "--no-vital-checks"
                       "-fexplicit"
                       "--ieee=synopsys"
                       local-file))))

;; for each regex listed in `auto-mode-alist' to enable `vhdl-mode',
;; also use that regex for `flymake-allowed-file-name-masks'.
(dolist (v auto-mode-alist)
  (when (eq (cdr v) 'vhdl-mode)
    (add-to-list 'flymake-allowed-file-name-masks '("\\.vhdl?\\'" flymake-ghdl-init))
    ))

;; the error line pattern for GHDL
(add-to-list 'flymake-err-line-patterns
             '("\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)" 1 2 3 4))

(setq flymake-ghdl-excludes '(":[0-9]+:10: [^\n]+ not found in library .work"
                              ":[0-9]+:14: [^\n]+ was not analysed"
                              ":[0-9]+:[0-9]+: no declaration for [^\n]+"))
(defun flymake-ghdl-exclude (output)
  "Rewrites GHDL error patterns listed in `flymake-ghdl-excludes'
to a dummy error expressions. This function is only useful if somehow
hooked or advised into the flymake library."
  (setq q output)
  (let* ((idx (length flymake-ghdl-excludes))
         (c 0)
         (out output))
    (while (< c idx)
      (while (not (null (string-match (nth c flymake-ghdl-excludes) out)))
        (setq out (replace-match ":0:00: GHDLdummy" nil nil out)))
      (setq c (+ 1 c)))
    out))

(defadvice flymake-parse-output-and-residual (before ghdl-flymake-parse-output-and-residual activate)
  "Advises `flymake-parse-output-and-residual' to preprocess the checker output with `flymake-ghdl-exclude'."
  (ad-set-arg 0 (flymake-ghdl-exclude (ad-get-arg 0))))

(defadvice flymake-highlight-line (around ghdl-flymake-highlight-line activate)
  "Advises `flymake-highlight-line' to perform its highlighting function if and only if
the current error line is not a rewritten dummy error."
  (let* ((args (ad-get-args 0))
         (fstr (car (nth 1 args)))
         (line (flymake-ler-line fstr))
         (msg  (flymake-ler-text fstr))
         (dum  (and msg (not (null (string-match "GHDLdummy" msg))))))
    (unless (and dum (= 0 line))
      ad-do-it)))

;;;;;;;;;;;; Emacs Flymake
(defun flymake-elisp-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "elisplint" (list local-file))))
(push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)
;(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;;;;;;;;;;;; Python Flymake
(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (if (eq window-system 'w32)
        (list "python" (list "c:\\Python26\\Scripts\\pyflakes" local-file))
      (list "pyflakes" (list local-file))
      )))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init))
;(add-hook 'find-file-hook 'flymake-find-file-hook)

(provide 'set-flymake)
