;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'vhdl-mode)

;; settings for VHDL mode
(setq vhdl-electric-mode nil)
(setq vhdl-stutter-mode t)
(setq vhdl-intelligent-tab nil)
(setq vhdl-indent-tabs-mode nil)

(setq-default vhdl-compiler "Cadence NC")
(setq-default vhdl-end-comment-column 120)

(setq-default vhdl-standard '(93 nil))
(setq vhdl-standard '(93 nil))
(setq-default vhdl-underscore-is-part-of-word t)
(setq vhdl-self-insert-comments nil)
(setq vhdl-actual-port-name '("\"\\(.*\\)$\"" . "s\\1"))
(setq vhdl-argument-list-indent nil)
(setq vhdl-insert-empty-lines 'none)
(setq vhdl-instance-name '(".*" . "\\&_inst_%d"))

(defadvice vhdl-mode (after effecting-vhdl-mode activate)
  "Electric tab is always as M-TAB instead of TAB."
  (local-set-key (kbd "M-TAB") 'vhdl-electric-tab)
  (local-set-key (kbd "M-q") 'vhdl-electric-tab)
  (local-set-key (kbd "TAB") 'indent-according-to-mode))

(defun my-as-vhdl-mode-hook ()
  ;; my customizations for vhdl-mode-map-init
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)) 
  (setq tab-width 4) 
  (setq indent-tabs-mode nil)  ; use spaces only if nil 
  (setq vhdl-indent-tabs-mode nil)
  (setq vhdl-basic-offset 4)
  )

(defun as-vhdl-style-on ()
  (interactive)
  (add-hook 'vhdl-mode-hook 'my-as-vhdl-mode-hook))
(defun as-vhdl-style-off ()
  (interactive)
  (remove-hook 'vhdl-mode-hook 'my-as-vhdl-mode-hook))

(provide 'set-vhdl-mode)
