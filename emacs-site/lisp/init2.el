;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; settings for headless web server

(defvar custom-dir "~/public/work/git/custom-emacs/emacs-site/lisp")
(defvar thirdparty-dir "~/public/work/git/custom-emacs/emacs-site/vendor")
(defvar snippets-dir "~/public/work/git/custom-emacs/emacs-site/snippets")
(defvar custom-file (substitute-in-file-name "$HOME/.emacs.d/custom.el"))
(setq custom-file (substitute-in-file-name "$HOME/.emacs.d/custom.el"))
(defvar laptop nil)
(defvar work nil)

(add-to-list 'load-path custom-dir)
(require 'load-all)

(global-auto-complete-mode nil)
