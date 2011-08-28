;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'mm-decode)
(require 'mm-view)

(defun effecting-mm-inline-render-with-file (handle post-func cmd &rest args)
  "Render file inline just like `mm-inline-render-with-file', except preserve unicode
output of command."
  (let ((source (mm-get-part handle)))
    (mm-insert-inline
     handle
     (progn
       (let ((file (mm-make-temp-file
                    (expand-file-name "mm" mm-tmp-directory))))
         (mm-with-unibyte-buffer
           (insert source)
           (let ((coding-system-for-write 'binary))
             (write-region (point-min) (point-max) file nil 'silent))
           (delete-region (point-min) (point-max)))
         (mm-with-multibyte-buffer
           (unwind-protect
               (apply 'call-process cmd nil t nil (mapcar 'eval args))
             (delete-file file))
           (and post-func (funcall post-func))
           (replace-string "" "" nil (point-min) (point-max)) ; cleanup pdftotext output
           (buffer-string))
         )))))

(setq effecting-mm-inline-media-tests
      '(
        ("application/msword" effecting-mm-inline-msword
         (lambda (handle) effecting-mm-inline-msword-binary))
        ("application/pdf" effecting-mm-inline-pdf
         (lambda (handle) effecting-mm-inline-pdf-binary))
        ("application/octet-stream" effecting-mm-inline-msword
         (lambda (handle) (effecting-mm-media-test-filename ".doc")))
        ("application/octet-stream" effecting-mm-inline-pdf
         (lambda (handle) (effecting-mm-media-test-filename ".pdf")))
        ("image/p?jpeg" mm-inline-image
         (lambda (handle) (mm-valid-and-fit-image-p 'jpeg handle)))
        ("image/png" mm-inline-image
         (lambda (handle) (mm-valid-and-fit-image-p 'png handle)))
        ))

(setq effecting-mm-inline-msword-binary (cond ((executable-find "wvCat") 'wvCat)
                                              ((executable-find "wvText") 'wvText)
                                              ((executable-find "antiword") 'antiword)
                                              ((executable-find "catdoc") 'catdoc)
                                              (t nil)))
(setq effecting-mm-inline-pdf-binary (cond ((executable-find "pdftotext") 'pdftotext)
                                           (t nil)))

(setq effecting-mm-image-scale-binary (cond ((executable-find "convert") 'convert)
                                            (t nil)))

(defun effecting-mm-inline-msword (handle)
  (cond ((eq effecting-mm-inline-msword-binary 'wvCat)
         (mm-inline-render-with-stdin handle nil "wvCat" "-dump-width" "80" "-"))
        ((eq effecting-mm-inline-msword-binary 'wvText)
         (effecting-mm-inline-render-with-file handle nil "sh" "-c" '(concat "wvText " file " /dev/fd/1"))); only works on Linux/UNIX
        ((eq effecting-mm-inline-msword-binary 'antiword)
         (mm-inline-render-with-stdin handle nil "antiword" "-"))
        ((eq effecting-mm-inline-msword-binary 'catdoc)
         ((mm-inline-render-with-stdin handle nil "catdoc" "-")))))

(defun effecting-mm-inline-pdf (handle)
  (cond ((eq effecting-mm-inline-pdf-binary 'pdftotext)
         (effecting-mm-inline-render-with-file handle nil "pdftotext" "-layout" 'file "-"))
        ))

;; (defun effecting-mm-inline-large-image (handle)
;;   (let ((b (point-marker))
;; 	buffer-read-only)
;;     (put-image (mm-get-image handle) b)
;;     (insert "\n\n")
;;     (mm-handle-set-undisplayer
;;      handle
;;      `(lambda ()
;; 	(let ((b ,b)
;; 	      buffer-read-only)
;; 	  (remove-images b b)
;; 	  (delete-region b (+ b 2)))))))

(setq effecting-mm-inline-image-scale-factor 70)

(defun effecting-mm-inline-image-scaled (handle)
  (let* ((b (point-marker))
         (i (mm-get-image handle))
         (buffer-read-only nil)
         (n (cons (car i)
                  (let* ((size (image-size i))
                         (w (car size))
                         (h (cdr size))
                         (mh (1- (window-height)))
                         (mw (1- (window-width)))
                         (dh (- w mw))
                         (dw (- h mh))
                         (s (cond ((and (< dh 0) (< dw 0)) nil)
                                  ((> dh dw) (/ (* 1.0 mh) h))
                                  ((t  (/ (* 1.0 mw) w)))))
                         (d (plist-get (cdr i) :data))
                         )
                    (if s (message "Scale image %dx%d to %d%s." w h (* effecting-mm-inline-image-scale-factor s) "%"))
                    (plist-put (cdr i) :data
                               (if s
                                   (mm-with-unibyte-buffer
                                     (insert d)
                                     (apply 'mm-inline-wash-with-stdin nil
                                            "convert"
                                            (list "-scale" (concat (format "%d" (* effecting-mm-inline-image-scale-factor s)) "%") "-" "-"))
                                     (buffer-string))
                                 d))
                      )))
         )
    (put-image n b)
    (insert "\n\n")
    (mm-handle-set-undisplayer
     handle
     `(lambda ()
	(let ((b ,b)
	      buffer-read-only)
	  (remove-images b b)
	  (delete-region b (+ b 2)))))))

(defadvice mm-inline-image (around effecting-mm-inline-image activate)
  (if effecting-mm-image-scale-binary
      (effecting-mm-inline-image-scaled (ad-get-arg 0))
    ad-do-it))

(defun effecting-mm-media-test-filename (handle extn)
  (let* ((type (mm-handle-type handle))
         (name-pair (assq 'name type))
         (name (cdr name-pair)))
    (if name (equal extn (substring name -4 nil)))))

;;;; Emacs MIME options

;; always check pgp sigs
(setq mm-verify-option 'always)
(setq mm-decrypt-option 'always)

;; image options
(setq mm-inline-large-images t)
(setq mm-inline-text-html-with-images t)

;; media tests - appending would be faster but not idempotent
(mapc (lambda (x) (add-to-list 'mm-inline-media-tests x))
      effecting-mm-inline-media-tests)

;; inline support
(mapc (lambda (x) (add-to-list 'mm-inlined-types x))
      '("application/msword"
        "application/pdf"
        "application/octet-stream"))
; inline images
(mapc (lambda (x) (add-to-list 'mm-attachment-override-types x))
      '("image/jpeg"
        "image/.*"))

;; avoid HTML where possible
(mapc (lambda (x) (add-to-list 'mm-discouraged-alternatives x))
       '("text/html"
         "text/richtext"))

;; HTML handling
;; It is unfortunate that Emacs 23 breaks compatibility with the old
;; emacs-w3m library. In Emacs 23, niceties such as italic and bold
;; text will not be visible.
(setq mm-text-html-renderer
      (cond ((executable-find "w3m")
             (if (and (locate-library "w3m") (< emacs-major-version 23))
                 'w3m
               'w3m-standalone))
            ((executable-find "links") 'links)
            ((executable-find "lynx") 'lynx)
            ((locate-library "w3") 'w3)
            ((locate-library "html2text") 'html2text)
            (t nil)))

(provide 'set-mm)
