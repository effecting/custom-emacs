;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; custom colors and faces

;; this requires emacs 22

;; (unless (<= emacs-major-version 21)
;;   (custom-set-faces
;;    '(erc-direct-msg-face ((t (:foreground "gold"))))
;;    '(erc-nick-msg-face ((t (:foreground "gold" :weight bold))))
;;    '(mode-line
;;      ((((class color) (min-colors 88) (background light))
;;        :box (:line-width -1 :color "grey85" :style nil)
;;        :foreground "black"
;;        :background "grey85")
;;       (((class color) (min-colors 88) (background dark))
;;        :box (:line-width -1 :color "grey30" :style nil)
;;        :foreground "white"
;;        :background "grey30")
;;       (t
;;        :inverse-video t)))
;;    '(mode-line-inactive
;;      ((default
;;         :inherit mode-line)
;;       (((class color) (min-colors 88) (background light))
;;        :box (:line-width -1 :color "grey95" :style nil)
;;        :background "grey95"
;;        :foreground "grey20")
;;       (((class color) (min-colors 88) (background dark))
;;        :box (:line-width -1 :color "grey10" :style nil)
;;        :background "grey10"
;;        :foreground "grey80")
;;       (t
;;        :inverse-video nil)))
;;    '(region
;;      ((((class color) (min-colors 88) (background dark))
;;        :background "slate blue")
;;       (((class color) (min-colors 88) (background light))
;;        :background "lightgoldenrod2")
;;       (((class color) (min-colors 16) (background dark))
;;        :background "blue3")
;;       (((class color) (min-colors 16) (background light))
;;        :background "lightgoldenrod2")
;;       (((class color) (min-colors 8))
;;        :background "blue" :foreground "white")
;;       (((type tty) (class mono))
;;        :inverse-video t)
;;       (t :background "gray"))))
;;   )

(provide 'conf-faces)
