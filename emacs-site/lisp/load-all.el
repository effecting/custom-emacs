;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; I used to add the newest VHDL mode, but it shamefully breaks other emacs modes.
;; Thus I just stick with the included mode from Emacs CVS.

;; load paths
(when (boundp 'thirdparty-dir)
  (add-to-list 'load-path thirdparty-dir)
  )
;;   (mapc (lambda (x) (add-to-list 'load-path (concat thirdparty-dir "/" x)))
;;         '(
;;           "dictionary-1.8.7"
;;           "vlog-mode-1.6"
;;           "icicles"
;;           "gnuserv-3.12.7"
;;           "epg-0.0.16"
;;           )))
(when (not (file-directory-p "~/.emacs.d/site-lisp"))
  (make-directory "~/.emacs.d/site-lisp"))
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(when (file-directory-p "~/.emacs.d/site-lisp/emacs-skype")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-skype"))
(when (file-directory-p "~/.emacs.d/site-lisp/emms/lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emms/lisp"))

(let ((my-maxima-emacs-dir (car (file-expand-wildcards "/usr/share/maxima/*/emacs"))))
  (if my-maxima-emacs-dir
      (add-to-list 'load-path my-maxima-emacs-dir)))

;; relocate customizations file
(if custom-file
    (load-file custom-file))

;; load settings that are UI-specific
(require (cond
          ((eq window-system 'x)   'conf-ui-x11)
          ((eq window-system 'w32) 'conf-ui-w32)
          ((eq window-system 'mac) 'conf-ui-aqua)
          (t                       'conf-ui-terminal)
          ))

;; presets
(require 'preset-startup)

;; various auto loads
(autoload 'abook "set-abook" nil t)
(autoload 'bitlbee-start "bitlbee" nil t)
(autoload 'cvs-status "pcvs" nil t)
(autoload 'dictionary "dictionary" nil t)
(autoload 'dictionary-lookup-definition "dictionary" nil t)
(autoload 'dictionary-match-words "dictionary" nil t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary" nil t)
(autoload 'dictionary-popup-matching-words "dictionary" nil t)
(autoload 'dictionary-search "dictionary" nil t)
(autoload 'dictionary-tooltip-mode "dictionary" nil t)
(autoload 'egit "egit" "Emacs git history" t)
(autoload 'egit-file "egit" "Emacs git history file" t)
(autoload 'egit-dir "egit" "Emacs git history directory" t)
(autoload 'erc-network-select "set-erc" nil t)
(autoload 'global-auto-complete-mode "set-auto-complete" nil t)
(autoload 'gnuserv-start "gnuserv-compat" nil t)
(autoload 'Info-find "set-info" nil t)
(autoload 'imath-mode "imath" nil t)
(autoload 'imaxima "imaxima" nil t)
(autoload 'imaxima-latex "imaxima-latex" nil t)
(autoload 'ledger "set-ledger" nil t)
(autoload 'maxima "maxima" nil t)
(autoload 'maxima-start "maxima" nil t)
(autoload 'my-emms-setup-auto "set-emms" nil t)
(autoload 'paredit-mode "paredit" nil t)
(autoload 'smart-chars-mode "smartchars" nil t)
(autoload 'svn-status "psvn" nil t)
(autoload 'w3m "w3m" nil t)
(autoload 'w3m-browse-url "w3m" nil t)
(autoload 'w3m-find-file "w3m" nil t)
(autoload 'effecting-mail-expand-abook "set-abook" nil t)
(autoload 'effecting-mail-expand-ldap "set-eudc" nil t)
(autoload 'effecting-recentf-find-file-concise "set-recentf" nil t)
(autoload 'woman-this "set-woman" nil t)
(autoload 'smart-operator-mode "smart-operator" nil t)
(autoload 'scilab-mode "scilab" nil t)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

; (require 'cl) ; for sake of `org'

;; various after loads with set-* libraries
(eval-after-load 'abook           '(require 'set-abook))
(eval-after-load 'auto-complete   '(require 'set-auto-complete))
(eval-after-load 'auto-complete-config '(require 'set-auto-complete))
(eval-after-load 'auctex          '(require 'set-tex))
(eval-after-load 'bitlbee         '(require 'set-bitlbee))
(eval-after-load 'bookmark        '(require 'set-bookmark))
(eval-after-load 'browse-url      '(require 'set-browse-url))
(eval-after-load 'calendar        '(require 'set-calendar))
(eval-after-load 'cc-mode         '(require 'set-cc-mode))
(eval-after-load 'comint          '(require 'set-comint))
(eval-after-load 'diary-lib       '(require 'set-calendar))
(eval-after-load 'dictionary      '(require 'set-dictionary))
(eval-after-load 'diff            '(require 'set-diff))
(eval-after-load 'ediff           '(require 'set-ediff))
(eval-after-load 'eldoc           '(require 'set-eldoc))
(eval-after-load 'elscreen        '(require 'set-elscreen))
(eval-after-load 'emms            '(require 'set-emms))
(eval-after-load 'epa             '(require 'set-epa))
(eval-after-load 'erc             '(require 'set-erc))
(eval-after-load 'eshell          '(require 'set-eshell))
(eval-after-load 'eudc            '(require 'set-eudc))
(eval-after-load 'footnote        '(require 'set-footnote))
(eval-after-load 'flymake         '(require 'set-flymake))
(eval-after-load 'flyspell        '(require 'set-flyspell))
(eval-after-load 'gnus            '(require 'set-gnus))
(eval-after-load 'gnuserv         '(require 'set-gnuserv))
(eval-after-load 'gnuserv-compat  '(require 'set-gnuserv))
(eval-after-load 'grep            '(require 'set-grep))
(eval-after-load 'info            '(require 'set-info))
(eval-after-load 'ispell          '(require 'set-ispell))
(eval-after-load 'latex           '(require 'set-tex))
(eval-after-load 'ledger          '(progn
                                     ;(require 'ledger-indent)
                                     (require 'set-ledger)))
(eval-after-load 'lpr             '(require 'set-lpr))
(eval-after-load 'maxima          '(require 'set-maxima))
(eval-after-load 'mh-e            '(require 'set-mh-e))
(eval-after-load 'mh-letter       '(require 'set-mh-e))
(eval-after-load 'mh-search       '(require 'set-mh-e))
(eval-after-load 'mm-decode       '(require 'set-mm))
(eval-after-load 'mm-view         '(require 'set-mm))
(eval-after-load 'mml             '(require 'set-mml))
(eval-after-load 'mml2015         '(require 'set-mml2015))
(eval-after-load 'mml-sec         '(require 'set-mml-sec))
(eval-after-load 'nroff-mode      '(require 'set-nroff-mode))
(eval-after-load 'org             '(require 'set-org))
(eval-after-load 'paredit         '(require 'set-paredit))
(eval-after-load 'post            '(require 'set-post))
(eval-after-load 'pgg             '(require 'set-pgg))
(eval-after-load 'recentf         '(require 'set-recentf))
(eval-after-load 'reftex          '(require 'set-reftex))
(eval-after-load 'scroll-bar      '(require 'set-scroll-bar))
(eval-after-load 'simple          '(require 'set-simple))
(eval-after-load 'server          '(require 'set-server))
(eval-after-load 'shell           '(require 'set-shell))
(eval-after-load 'skype           '(require 'set-skype))
(eval-after-load 'smart-operator  '(require 'smart-operator))
(eval-after-load 'speedbar        '(require 'set-speedbar))
(eval-after-load 'term            '(require 'set-term))
(eval-after-load 'tex             '(require 'set-tex))
(eval-after-load 'time            '(require 'set-time))
(eval-after-load 'timeclock       '(require 'set-timeclock))
(eval-after-load 'timeclock-x     '(require 'set-timeclock))
(eval-after-load 'tramp           '(require 'set-tramp))
(eval-after-load 'vc              '(require 'set-vc))
(eval-after-load 'vc-git          '(progn
                                     (require 'git)
                                     (require 'git-blame)
                                     ))
(eval-after-load 'vhdl-mode       '(require 'set-vhdl-mode))
(eval-after-load 'vlog-mode       '(require 'set-vlog-mode))
(eval-after-load 'w3m             '(require 'set-w3m))
(eval-after-load 'w3m-search      '(require 'set-w3m))
(eval-after-load 'woman           '(require 'set-woman))
(eval-after-load 'xt-mouse        '(require 'set-xt-mouse))
(eval-after-load 'yasnippet       '(require 'set-yasnippet))

;; custom keys pertaining to auto loads
(global-set-key (kbd "s-i") 'erc-network-select)
(global-set-key (kbd "s-e s-e") 'my-emms-setup-auto)
(global-set-key (kbd "C-x m") 'mh-index-new-messages)
(global-set-key (kbd "C-x M-m") 'mh-visit-folder)
(global-set-key (kbd "C-x M-RET") 'mh-smail)
(global-set-key (kbd "C-x g") 'gnus)
(global-set-key (kbd "C-c C-i") 'ispell-buffer)
(global-set-key (kbd "C-c M-i") 'ispell-comments-and-strings)
(global-set-key (kbd "C-x M-d") 'svn-status)
(global-set-key (kbd "C-x r f") 'effecting-recentf-find-file-concise)
(global-set-key (kbd "C-x r v") 'effecting-recentf-view-file-concise)
(global-set-key (kbd "C-x r C-f") 'effecting-recentf-find-file-concise)
(global-set-key (kbd "C-M-$") 'ispell-region)

; setup MH as interal mail, only if MH profile installed
(when (and (file-exists-p "~/.mh_profile")
           (locate-library "mh-loaddefs"))
  
  (require 'mh-loaddefs)
  ;; configure Emacs to use MH-E
  (setq mail-user-agent 'mh-e-user-agent)
  ;; alias rmail modes to MH-E
  (defalias 'rmail 'mh-rmail)
  (defalias 'smail 'mh-smail)
  (defalias 'mail 'mh-smail)
  )

; force w3m to allow use of 23 if and only if necessary
(when (and (>= emacs-major-version 23)
           (locate-library "w3m-e21")
           (not (locate-library "w3m-e23")))
  (require 'w3m-e21)
  (provide 'w3m-e23))

;; custom options pertaining to auto loads
(require 'conf-emacs)
(require 'conf-files)
(require 'conf-buffers)
(require 'conf-windows)
(when window-system
  (require 'conf-faces)
  (require 'conf-hooks)
  )

(defun my-load-layout-prereq ()
  ;; some auto-conversions for `view-file'
  (when (locate-library "txutils")
    (require 'txutils))

  ;; track recent files
  (require 'recentf)

  ;; save buffer place
  (require 'saveplace)
  (setq-default save-place t)

  ;; personal miscellanea
  (require 'my-misc))

(defun my-load-layout1 ()
  (interactive)
  (my-load-layout-prereq)
  (load "load-layout1"))
(defun my-load-layout2 ()
  (interactive)
  (my-load-layout-prereq)
  (load "load-layout2"))
(defun my-load-layout3 ()
  (interactive)
  (my-load-layout-prereq)
  (load "load-layout3"))
(defun my-load-layout4 ()
  (interactive)
  (my-load-layout-prereq)
  (load "load-layout4"))
(defun my-load-layout5 ()
  (interactive)
  (my-load-layout-prereq)
  (load "load-layout5"))

;; layout levels
(global-set-key (kbd "s-1") 'my-load-layout1)
(global-set-key (kbd "s-2") 'my-load-layout2)
(global-set-key (kbd "s-3") 'my-load-layout3)
(global-set-key (kbd "s-4") 'my-load-layout4)
(global-set-key (kbd "s-5") 'my-load-layout5)

(provide 'load-all)
