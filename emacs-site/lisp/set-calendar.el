;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

; settings for calendar and diary

(require 'calendar)
(require 'diary-lib)
(require 'cal-dst)

(setq abbreviated-calendar-year nil)

(setq calendar-time-zone (* -5 60))
(setq calendar-standard-time-zone-name "EST")
(setq calendar-daylight-time-zone-name "EDT")

(setq calendar-daylight-savings-start '(calendar-dst-starts year))
(setq calendar-daylight-savings-ends '(calendar-dst-ends year))
(setq calendar-daylight-time-offset 60)

(setq calendar-dst-check-each-year-flag t)

(setq calendar-latitude 43.1)
(setq calendar-longitude -77.6)
(setq calendar-location-name "Rochester, NY")

(setq mark-diary-entries-in-calendar t)
(setq view-diary-entries-initially nil)

(setq european-calendar-style nil)
(setq calendar-date-display-form american-calendar-display-form)
(setq diary-date-forms american-date-diary-pattern)

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

(provide 'set-calendar)
