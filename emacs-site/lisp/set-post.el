;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'post)

;; yes, I may want this, but only for actual composing
;; disable here and do myself if not read-only
(setq post-rename-buffer nil)

(defadvice post-mode (before before-post-mode activate)
  ;; it is silly to change the buffer name if opened read-only
  )

(defun post-view-kill ()
  "Quit View mode, kill current buffer."
  (interactive)
  (let* ((b (current-buffer))
         (n (buffer-name b)))
    
    ; The following line still does not always work;
    ; sometimes View just toggles the read-only status.
    ;(view-mode-exit view-return-to-alist 'kill-buffer t)
    ;;     (let ((x (get-buffer n)))
    ;;       (if x
    ;;           (kill-buffer x)))

    (view-mode-exit view-return-to-alist nil t)
    (let ((x (buffer-list)))
      (while x
        ;;(message "Looking at buffer %s" (buffer-name (car x)))
        (when (string-match (regexp-quote n) (buffer-name (car x)))
          (message "Clean post-mode buffer %s" (buffer-name (car x)))
          (kill-buffer (car x))
          )
        (setq x (cdr x))))
    ))

(defun post-rename-buffer-now ()
  "Rename the buffer on demand. To be called by a shell script."
  ;; do not setq post-buf here, it seems unsafe
  (rename-buffer "*Mutt Composing*" t))

(defvar post-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'post-view-kill)
    map))

(defadvice post-mode (after after-post-mode activate)
  "When in post-mode, remap minor mode `view-mode' key \"q\"
to actually kill the buffer every time. No guessing games, please."
  (add-to-list 'minor-mode-overriding-map-alist (cons 'view-mode post-view-mode-map))
  )

(provide 'set-post)
