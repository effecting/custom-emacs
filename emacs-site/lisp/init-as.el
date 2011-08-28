;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; settings for work servers

(defvar custom-dir "d:/svn_work/emacs-site/lisp")
(defvar snippets-dir "d:/svn_work/emacs-site/snippets")
(defvar thirdparty-dir "d:/svn_work/emacs-site/vendor")
(defvar custom-file (substitute-in-file-name "$HOME/.emacs.d/custom.el"))
(setq custom-file (substitute-in-file-name "$HOME/.emacs.d/custom.el"))
(defvar laptop nil)
(defvar work t)

(add-to-list 'load-path custom-dir)
(eval-after-load 'set-cc-mode         '(as-c-style-on))
(eval-after-load 'set-vhdl-mode       '(as-vhdl-style-on))
(require 'load-all)

(global-auto-complete-mode t)
