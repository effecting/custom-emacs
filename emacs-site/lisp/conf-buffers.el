;; 
;; 

;; Authored by effecting2 in 2006-2007.2007.

;; This code is not part of GNU Emacs.

;; Some of this source modeled after examples by:
;;     Kin Cho (kin@neoscale.com)
;;
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information. 

;; Note that there are a lot of tiny defadvice statements in here,
;; and also in `set-bookmark', `set-recentf', ...
;;
;; Many Lisp authors would use the CL `flet' macro to perform the
;; same functionality. Unfortunately, `flet' violates the dynamic
;; scoping of Emacs and is only available for those programmers who
;; do not what to write "correct" Emacs code. Personally, I am a pedant
;; about correct code (up to my current level of knowledge), and so my
;; implementations are just as concise without any use of `flet'.

;; Additionally, the `iswitchb' hook comment is very misleading in
;; it suggests the hook is run after prompting for a buffer. In fact,
;; the hook changes the prompt contents entirely, making it useful.
;; This permits an even cleaner approach than the use `defadvice' for
;; simple, prompt-once cases.

;; various modes for buffer management

;; find file at point
(when (locate-library "ffap")
  (require 'ffap)
  (ffap-bindings))

;; use smart interactive buffer selection
(when (locate-library "iswitchb")
  (require 'iswitchb) ;; just load now
  ;;(autoload 'iswitchb-buffer "iswitchb" nil t)
  ;;(autoload 'iswitchb-minibuffer-setup "iswitchb")
  (add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)
  ;; always switch in current window, no assuming what I want
  (setq iswitchb-default-method 'samewindow)
  (global-set-key (kbd "C-x b") 'iswitchb-buffer)
  (global-set-key (kbd "C-x 4 b") 'iswitchb-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") 'iswitchb-buffer-other-frame)
  ;; original commands
  (global-set-key (kbd "C-x M-b") 'switch-to-buffer)
  (global-set-key (kbd "C-x 4 M-b") 'switch-to-buffer-other-window)
  (global-set-key (kbd "C-x 5 M-b") 'switch-to-buffer-other-frame)

  (defun my-text-string-abbrev (x)
    "Create an abbreviation of a long string of arbitrary characters."
    (let ((y x))
      (while (not (null (string-match "[\t\n\r]+\\|\\s \\s +" y)))
        (setq y (replace-match " " nil nil y)))
      (if (> (length y) 400) (substring y 0 400) y)
      ))
  (defun my-text-ring-alist (r)
    "Provide `my-yank-select' with an alist of
 (abbreviation . filename) ."
    (require 'cl) ;; FIXME -- I don't want to use CL for `copy-list'
    (let* ((kr (copy-list r))
           (al (mapcar (lambda (x)
                         (let ((y (substring-no-properties x)))
                           (cons (my-text-string-abbrev y) x)))
                       kr))
           (cal (mapcar 'car al)))
      
      al
      ))
  (defun my-yank-select (&optional text)
    "Use the `iswitchb' interface to `yank' an item from
recent entries in the `kill-ring'."
    (interactive (list (let* ((lkr (length kill-ring))
                              (kr (butlast kill-ring (cond ((> lkr 20) (- lkr 20) 0))))
                              (al (my-text-ring-alist kr))
                              (iswitchb-make-buflist-hook (lambda ()
                                                            (setq iswitchb-temp-buflist (mapcar 'car al))
                                                            )))
                         (cdr (assoc (iswitchb-read-buffer "yank ") al))
                         )))
    (insert-for-yank text)
    )

  (global-set-key (kbd "C-M-y") 'my-yank-select)
  )
;; use smart interactive buffer listing
(when (locate-library "ibuffer")
  (setq ibuffer-default-sorting-mode 'major-mode)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-x C-M-b") 'list-buffers) ; original command
  )


;; do not save places ; to be overridden by loading a layout
(setq-default save-place nil)

;; convience bindings
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)

(provide 'conf-buffers)
