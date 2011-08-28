;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

(require 'w3m)
(require 'w3m-search)

;; use w3m as default browser
(setq w3m-key-binding 'info)
(setq w3m-mode-map w3m-info-like-map)
(setq w3m-minor-mode-map (w3m-make-minor-mode-keymap))

;; no decorations, please
(mapc (lambda (v) (set v nil))
      '(
        w3m-show-graphic-icons-in-header-line
        w3m-show-graphic-icons-in-mode-line
        w3m-toggle-inline-images-permanently
        w3m-track-mouse
        w3m-use-favicon
        w3m-use-header-line
        w3m-use-tab-menubar
        w3m-use-toolbar
        ))

;; other options
(setq w3m-use-cookies t)
(setq w3m-default-save-directory "~/Downloads/")

;; 'open' is my own auto-viewing command that uses KDE, GNOME, or XFCE
;; subsystem to detect MIME and open -- typically use XFCE
(setcdr (assoc "application/pdf" w3m-content-type-alist) `("\\.pdf\\'" ("open" file) nil))

;; make frames work by using w3m variant when available
;; (let ((w3mmee (executable-find "w3mmee"))
;;       (mbconv (executable-find "mbconv")))
;;   (when (and w3mmee mbconv)
;;     (setq w3m-command w3mmee)
;;     (setq w3m-type 'w3mmee)))

;; add emacs wiki to search engines
(mapc (lambda (x) (add-to-list 'w3m-search-engine-alist x))
      '(
        ("emacs-wiki"      "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s")
        ("ebay"            "http://search.ebay.com/search/search.dll?query=%s")
        ("google-groups"   "http://www.google.com/groups?q=%s")
        ("weather"         "http://www.weather.com/search/search?where=%s&what=WeatherLocalUndeclared")
        ("wikipedia-en"    "http://en.wikipedia.org/wiki/Special:Search?search=%s")
        ))

;; make the selected engine persistent for this session (only)
(defadvice w3m-search (after change-default activate)
  (let ((engine (nth 1 minibuffer-history)))
    (when (assoc engine w3m-search-engine-alist)
      (setq w3m-search-default-engine engine))))

(provide 'set-w3m)
