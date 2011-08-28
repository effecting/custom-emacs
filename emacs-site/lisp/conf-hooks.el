;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; hooks for auto-fill
(mapc (lambda (x)
        (add-hook x 'turn-on-auto-fill))
      '(
        text-mode-hook
        org-mode-hook
        post-mode-hook
        ))

;; enable eldoc for select major modes
(mapc (lambda (x) (add-hook x 'turn-on-eldoc-mode))
      '(
        emacs-lisp-mode-hook
        ielm-mode-hook
        hexl-mode-hook
        ))
;; enable eldoc for select major modes, not on W32
(when (not (eq window-system 'w32))
  (mapc (lambda (x) (add-hook x 'turn-on-eldoc-mode))
        '(
          python-mode-hook
          )))

(mapc (lambda (x)
        (add-hook x 'turn-on-flyspell))
      '(
        html-mode-hook
        ;;          latex-mode-hook ;; latex inherits from something else (text mode?)
        xml-mode-hook
        nxml-mode-hook
        mh-letter-mode-hook
        ))

(mapc (lambda (x)
        (add-hook x 'footnote-mode))
      '(
        mh-letter-mode-hook
        mh-show-mode-hook
        ))

(mapc (lambda (x)
        (add-hook x 'longlines-mode))
      '(
        ))

(when (locate-library "emaxima")
  (autoload 'emaxima-mark-file-as-emaxima "emaxima" nil t)
  (add-hook 'emaxima-mode-hook 'emaxima-mark-file-as-emaxima))

(when (locate-library "paredit")
  (mapc (lambda (x)
          (add-hook x 'paredit-mode))
        '(
          ;; emacs-lisp-mode-hook
          ielm-mode-hook
          ;; lisp-mode-hook
          ;; slime-mode-hook
          inferior-lisp-mode-hook
          ;; scheme-mode-hook
          inferior-scheme-mode-hook)))

;; try to load slime for emacs lisp interaction
(when (locate-library "slime")
  (autoload 'slime-mode "slime" nil t)
  (add-hook 'lisp-interaction-mode-hook 'slime-mode))

;; LATEX hooks
(when (locate-library "reftex")
  (autoload 'turn-on-reftex "reftex" nil t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;; (when (locate-library "imath")
;;   (add-hook 'LaTeX-mode-hook 'imath-mode) )

;; (when (locate-library "preview")
;;   (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
;;   (autoload 'LaTeX-preview-setup "preview" nil t)
;;   (defvar preview-icondir (concat (file-name-directory (locate-library "preview")) "images")) )

;; (when (locate-library "org")
;;   (add-hook 'org-mode-hook 'longlines-mode) )

;; flymake for vhdl-mode
;; (when (executable-find "ghdl")
;;   (add-hook 'vhdl-mode-hook 'flymake-mode))

(provide 'conf-hooks)
