;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; clisp editing
(defvar clisp-mode-loaded nil)

;; try to load clisp emacs settings should lisp-mode be used
(defun clisp-mode-load ()
  (when (locate-library "clisp-ffi")
    (load "clisp-ffi"))
  (when (locate-library "clisp-coding")
    (load "clisp-coding"))
  )

(define-derived-mode clisp-mode lisp-mode "C Lisp"
  "Major mode for editing Clisp lisp files."
  (when (null clisp-mode-loaded)
    (clisp-mode-load) )
  (when (locate-library "clisp-indent")
    (require 'clisp-indent))
  (when (locate-library "clhs")
    (require 'clhs))
  (setq clisp-mode-loaded t)
  )

(provide 'clisp-mode)
