;; 
;; 

;; Authored by effecting2 in 2011.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'anything)
(require 'skype)

(add-hook 'skype--message-mode-hook 'turn-on-flyspell)

(provide 'set-skype)
