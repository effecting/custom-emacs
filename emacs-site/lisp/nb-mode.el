;; 
;; 

;; Authored by effecting2 in 2006-2007.2007.
;;
;; nb-mode.el is a Major Mode for Emacs intended for editing
;; NanoBlogger blog entries in plain text.
;;
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information. 

;; This code is not part of GNU Emacs.

;; Commentary:
;;
;; An appropriate expresion for adding an entry to auto-mode-alist:
;;
;; (add-to-list 'auto-mode-alist (cons
;;                                "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9T]\\{5\\}_[0-9]\\{2\\}_[0-9]\\{2\\}\\.txt\\'"
;;                                'NB-mode))
;;

;; POTENTIAL TODO
;;
;; - require NanoBlogger's end directive:  END-----
;; - read-only and colored header fields and force FORMAT value of muse
;;   (on open? or on save? or by custom?)
;;      TITLE: 
;;      AUTHOR:
;;      DATE: 
;;      DESC: 
;;      FORMAT: muse
;;      -----
;;      BODY:
;; - custom to select parent major mode
;;   (requires defines NB-mode in detail
;;    instead of using `define-derived-mode')
;;

(if (locate-library "muse-autoloads")
    (require 'muse-autoloads)
  (progn
    (require 'muse)
    (require 'muse-mode)
    (require 'muse-publish)
    (require 'muse-html)
    ))

(defgroup NB-mode nil
  "Composing NanoBlogger posts NB-mode.
You can get NanoBlogger from http://nanoblogger.sourceforge.net/."
  :group 'outlines
  :group 'hypermedia)

(defcustom NB-tab-width 8
  "A positive value specifies the default TAB width in number of spaces."
  :type 'number
  :group 'NB-mode)

(defcustom NB-untabify-on-save 1
  "Positive means, NB-mode should untabify the buffer before saving.
Negative means, NB-mode should tabify the buffer before saving.
Zero or nil means, NB-mode should not modify the buffer tabs before saving."
  :type '(choice
          (const :tag "Untabify" 1)
          (const :tag "Tabify" -1)
          (const :tag "Do nothing" nil))
  :group 'NB-mode)

(defcustom NB-enable-fill-mode t
  "Non-nil means, enable auto-fill for NB-mode by default."
  :type 'boolean
  :group 'NB-mode)

(defcustom NB-inhibit-kill-emacs t
  "Prevent C-c C-c from ever chosing to kill the emacs process."
  :type 'boolean
  :group 'NB-mode)

(defun NB-untabify-buffer ()
  "Untabify, tabify, or do nothing to the whole buffer,
as configured with customization variable `NB-untabify-on-save'."
  (if (> NB-untabify-on-save 0)
      (untabify (buffer-end -1) (buffer-end +1))
    (if (< NB-untabify-on-save 0)
        (tabify (buffer-end -1) (buffer-end +1)))))

(defun NB-gnuserv-edit-save (&optional count)
  "Save the current buffer and finish gnuserv edit.
The optional argument is passed to `gnuserv-edit'"
  (interactive "P")
  (save-buffer 0) ; no backup file
  (gnuserv-edit count))

(defun NB-server-edit-save (&optional arg)
  "Save the current buffer and finish server edit.
The optional argument is passed to `server-edit'"
  (interactive "P")
  (save-buffer 0) ; no backup file
  (server-edit arg))

(defun NB-save-and-kill-emacs (&optional arg)
  "Save the current buffer and kill emacs. The optional argument
is passed to `save-buffer'"
  (interactive "P")
  (save-buffer (if (null arg) 1 arg))
  (when (not NB-inhibit-kill-emacs)
    (save-buffers-kill-emacs)))

(defun NB-edit (&optional arg)
  "Determine whether the current buffer is a client buffer
and how to close the buffer. Calls one of:
`NB-gnuserv-edit-save' `NB-server-edit-save' `NB-save-and-kill-emacs'.
If the buffer is connected to a server process, or the variable
`NB-inhibit-kill-emacs' is non-nil, the buffer will be saved, but emacs
will not be exited."
  (interactive "P")
  (if (and (boundp 'gnuserv-minor-mode) gnuserv-minor-mode)
      (NB-gnuserv-edit-save arg)
    (if (and (boundp 'server-mode) server-mode)
        (NB-server-edit-save arg)
      (NB-save-and-kill-emacs arg))))

(define-derived-mode NB-mode muse-mode "NB"
  "Major mode for editing NanoBlogger entries."
  :group 'NB-mode
  (message buffer-file-name)
  (setq mode-name (if (string-match "nb_edit-newentry-" (buffer-file-name))
                      "NB-New"
                    "NB-Edit"))
  (set (make-local-variable 'require-final-newline) t)
  (auto-fill-mode (if NB-enable-fill-mode 1 0))
  (make-local-variable 'tab-width)
  (setq tab-width (if (> 0 NB-tab-width) NB-tab-width 8))
  (add-hook 'write-contents-hooks 'NB-untabify-buffer)
  (local-set-key (kbd "C-c C-c") 'NB-edit)
  )

;; lowercase alias as is convention
(defalias 'nb-mode 'NB-mode)
