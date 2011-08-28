;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; settings for laptop

(defvar custom-dir "~/pubvc/elisp-snippets")
(defvar thirdparty-dir "~/svn_work/nix/site-lisp")
(defvar custom-file (substitute-in-file-name "$HOME/.emacs.d/custom.el"))
(setq custom-file (substitute-in-file-name "$HOME/.emacs.d/custom.el"))
(defvar laptop t)
(defvar work t)

(add-to-list 'load-path custom-dir)
(require 'load-all)
