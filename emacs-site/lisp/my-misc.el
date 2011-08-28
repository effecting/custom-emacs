;; 
;; 

;; Authored by effecting2 in 2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.
(defun ssh (hostname)
  "Start a 'term' running ssh to hostname."
  (interactive (list (read-from-minibuffer "SSH Host: " "")))
  (if (> (length hostname) 0)
      (let ( (name (format "ssh: %s" hostname))
             (nstar (format "*ssh: %s*" hostname)) )

        (set-buffer (make-term name "ssh" nil hostname))
        (term-mode)
        (term-char-mode)
        (switch-to-buffer nstar)
        )))

(global-set-key (kbd "s-s") 'ssh)

(defun mutt-embed ()
  "Start a 'term' running mutt."
  (interactive)
  (set-buffer (make-term "mutt" "mutt.sh" nil ""))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*mutt*")
  )

(defun mutt (&optional geom)
  "Start a urvxtc terminal running mutt."
  (interactive)
  (if (eq nil geom)
      (urxvtc "128x11" "mutt.sh")
    (urxvtc geom "mutt.sh")) )

(defun fetchmail_loop ()
  "Start a fetchmail loop."
  (interactive)
  (if (or (equal system-name "pedahzur.rslink") (equal system-name "pedahzur") (equal system-name "pedahzur.local"))
      (comint-run "fetchmail_loop.sh")))

(defun fetchmail ()
  "An alias for fetchmail_loop."
  (interactive)
  (fetchmail_loop))
  
(defun mairix_loop ()
  "Start a mairix loop."
  (interactive)
  (if (or (equal system-name "pedahzur.rslink") (equal system-name "pedahzur"))
      (comint-run "mairix_loop.sh")))

(setq urxvt-default-geometry "80x55")

(setq urxvt-start-daemon nil)

(defun urxvtc (&optional geom exec)
  "Start a urvxtc terminal."
  (interactive)
  (when (and urxvt-start-daemon (not (eq 0 (shell-command "pgrep urxvtd >/dev/null 2>/dev/null"))))
    (if (eq 0 (shell-command "sh -c 'urxvtd -q -f -o >/dev/null 2>/dev/null </dev/null'"))
        (message "Urxvtd daemon started")
      (error "Not sure if urxvtd daemon really started") ))
  (let ( (opts (concat " --geometry "
                       (if (eq geom nil) urxvt-default-geometry geom)
                       (if (eq exec nil) " " (concat " -e " exec)))
                      ) )
    (if (eq 0 (shell-command (concat "urxvtc" opts)))
        (message "Urxvtc terminal started")
      (when t
        (setq urxvt-server-loaded nil)
        (error "Unable to start urxvtc; please try again"))) ))

(defun python ()
  (interactive)
  (comint-run "python"))

(defun wcalc ()
  (interactive)
  (comint-run "wcalc"))

(defun mpd-start ()
  "custom load my mpd settings for MPD"
  (interactive)
  ;;(kill-buffer "*mpd.sh*")
  (comint-run "mpd.sh")
  )

(defun effecting-highlight-datasheet ()
  "Highlights for datasheets converted to text by wvText"
  (interactive)
  (highlight-lines-matching-regexp "Register.*address:" 'hi-green)
  (highlight-phrase "Default:" 'hi-pink)
  (highlight-phrase "Bit.\\{0,20\\}[0-9]+ *:" 'hi-pink)
  (highlight-phrase "[+|-]\\{8,1000\\}" 'hi-blue)
  (highlight-phrase "[|]" 'hi-blue)
  (highlight-lines-matching-regexp "     [0-9]\\{1,3\\}.  ?[A-Z/].*" 'hi-yellow)
  )

;; setup word general expansions for a specific mode
(defun effecting-expand-abbrev (arg))
(defun effecting-expand (&optional prefix-arg)
  "Perform `hippie-exp' expansion on the current word. Expansion
is performed for matching words across all open buffers of the same
mode as the current buffer."
  (interactive "P")
  (require 'hippie-exp)
  ;; create definition for expansion function on the first call
  (when (not (commandp 'effecting-expand-abbrev))
    (fset 'effecting-expand-abbrev (make-hippie-expand-function
                                 '(try-expand-dabbrev
                                   try-expand-dabbrev-all-buffers))))
    
  (let ((case-fold-search nil)
        (case-replace nil)
        (hippie-expand-only-buffers
         (or (and (boundp 'hippie-expand-only-buffers)
                  hippie-expand-only-buffers)
             (list major-mode))))
       (effecting-expand-abbrev prefix-arg)))

(provide 'my-misc)

(setq effecting-thesis-dir "~/svn_work/rit/0306890/")
(setq effecting-thesis-list
      (mapcar (lambda (x)
              (expand-file-name (concat effecting-thesis-dir x)))
            '(
             ""
             "thesis/"
             "proposal/"
             "thesis/0306-890-thesis.tex"
             "references/glossary.bib"
             "thesis/secs/*.tex"
             "worklog/*.org"
             "projects/04_framebuf_frext_emul/src/"
             )))
(setq effecting-thesis-list-pdfs
      (mapcar (lambda (x)
              (expand-file-name (concat effecting-thesis-dir x)))
            '(
              "references/ieee/01218189.pdf"
              "references/spie/psi000454.pdf"
              "references/misc/warsaw_thesis_final.pdf"
              "references/itu/itu_t_rec_h264_200503.pdf"
             )))

(defun effecting-thesis-find-files (&optional prefix-arg)
  "Edit thesis documents and files."
  (interactive "P")
  (mapc (lambda (x)
          (find-file x t))
        effecting-thesis-list))

(defun effecting-thesis-open-pdfs (&optional prefix-arg)
  "Open thesis PDF reference documents."
  (interactive "P")
  (mapc (lambda (x)
          (shell-command (concat "open " x)))
        effecting-thesis-list-pdfs))

(defun effecting-insert-datestamp (&optional arg)
  "Insert a date stamp at point."
  (interactive "P")
  (insert (format-time-string
           (if arg
               "%Y-%m-%d @ %H:%M"
             "%Y-%m-%d")
           )))

(defun effecting-wgetpaste-region (start end)
  "Call \"wgetpaste\" on region and place URL on kill-ring."
  (interactive
   (list (region-beginning)(region-end)))
  (let ((s (buffer-substring-no-properties start end)))
  (with-temp-buffer
    (insert s)
    (shell-command-on-region (buffer-end -1) (buffer-end 1) "wgetpaste" t t)
    (search-forward "http://")
    (search-backward "http://")
    (copy-region-as-kill (point) (buffer-end 1))
    )))

(defun tmp-thesis1 ()
  (interactive)
  (while (search-forward-regexp "\\([0-9]+\\) *ps")
    (replace-match 
     (concat (number-to-string
              (round (* (/ 1 (/ (string-to-number (match-string 1)) (expt 10 12.0))) 8007)))
             " &")
      )))

(defun tmp-thesis2 ()
  (interactive)
  (while (search-forward-regexp "^\\([0-9]+\\) +\\([0-9]+\\) *$")
    (replace-match 
     (concat (format "%.3f"
              (/ (* 1.0
                    (string-to-number (match-string 1))
                    (string-to-number (match-string 2)))
                 (expt 2 30.0)))
             "")
      )))

(defun tmp-thesis3 ()
  (interactive)
  (while (search-forward-regexp "\\([0-9]+\\) *ps")
    (replace-match 
     (concat (number-to-string
              (* (/ 1 (/ (string-to-number (match-string 1)) (expt 10 12.0))) 6))
             " &")
      )))

(defun tmp-thesis4 ()
  (interactive)
  (while (search-forward-regexp "\\([0-9]+\\)[$]")
    (search-backward-regexp "\\([0-9]+\\)[$]")
    (replace-match 
     (concat (format "%.2f" (* (string-to-number (match-string 1)) 3) "$")))))
