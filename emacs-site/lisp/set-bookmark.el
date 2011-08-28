;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'bookmark)
(require 'iswitchb)
;; make all bookmark minibuffer functions use iswitchb style

;; This is simple as hook is available.
;; Using an advice has the benefit of working for
;; all functions that call the same read function, including:
;; `bookmark-delete', `bookmark-jump', ...
;;
(defadvice bookmark-completing-read (around around-bookmark-completing-read activate)
  (let ((args (ad-get-args 0))
        (iswitchb-make-buflist-hook (lambda () (setq iswitchb-temp-buflist (bookmark-all-names)))))
    (setq ad-return-value (iswitchb-read-buffer (concat (nth 0 args) " ") (nth 1 args)))
    ))

;; An alternate implementation, without hook;
;; a more correct way to avoid `flet'. A good
;; template for when the hook is insufficient.
;;
;;   (defadvice iswitchb-make-buflist (around bookmark-iswitchb-make-buflist activate)
;;     (if (and (boundp 'my-bookmark-now) my-bookmark-now)
;;         (setq iswitchb-buflist (bookmark-all-names))
;;       ad-do-it))
;;   (defadvice bookmark-completing-read (around around-bookmark-completing-read activate)
;;     (let ((my-bookmark-now t)
;;           (args (ad-get-args 0)))
;;       (setq ad-return-value (iswitchb-read-buffer (concat (nth 0 args) " ") (nth 1 args)))
;;       ))

(provide 'set-bookmark)
