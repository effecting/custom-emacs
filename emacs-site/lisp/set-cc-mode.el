;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'cc-mode)
(defun my-vs-c-mode-common-hook () 
        ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode 
        (c-set-offset 'substatement-open 0) 
        ;; other customizations can go here 
 
        (setq c++-tab-always-indent t) 
        (setq c-basic-offset 4)                  ;; Default is 2 
        (setq c-indent-level 4)                  ;; Default is 2 
 
        (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)) 
        (setq tab-width 4) 
        (setq indent-tabs-mode t)  ; use spaces only if nil 
) 

(defun my-as-c-mode-common-hook () 
        ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode 
        (c-set-offset 'substatement-open 0) 
        ;; other customizations can go here 
 
        (setq c++-tab-always-indent t) 
        (setq c-basic-offset 4)                  ;; Default is 2 
        (setq c-indent-level 4)                  ;; Default is 2 
 
        (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)) 
        (setq tab-width 4) 
        (setq indent-tabs-mode nil)  ; use spaces only if nil 
) 

(defun visual-studio-on ()
  (interactive)
  (add-hook 'c-mode-common-hook 'my-vs-c-mode-common-hook))
(defun visual-studio-off ()
  (interactive)
  (remove-hook 'c-mode-common-hook 'my-vs-c-mode-common-hook))
(defun as-c-style-on ()
  (interactive)
  (add-hook 'c-mode-common-hook 'my-as-c-mode-common-hook))
(defun as-c-style-off ()
  (interactive)
  (remove-hook 'c-mode-common-hook 'my-as-c-mode-common-hook))
(provide 'set-cc-mode)
