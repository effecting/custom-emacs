;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; minimal mode for inital *scratch* to reduce autoloading
(setq initial-major-mode 'fundamental-mode)

;; seed randomization
(random t)

;; settings
(setq-default fill-column 78)                 ;; wrap at approx 80 chars
(setq-default indent-tabs-mode nil)           ;; no tab for indentation
(setq inhibit-splash-screen t)                ;; no greeting
(setq case-fold-search t)                     ;; case-insensitive search
(setq save-abbrevs 'silently)                 ;; don't prompt to save abbrev
(setq require-final-newline t)                ;; require \n before EOF
(setq auto-save-interval 300)                 ;; autosave characters interval

;; modes enabled by default
(mapc (lambda (x) (when (fboundp x) (funcall x +1)))
      '( 
        global-font-lock-mode                 ;; colors
        transient-mark-mode                   ;; visible mark region
        delete-selection-mode                 ;; delete marked area with BKSPC
        auto-compression-mode                 ;; handle .gz, .bz2, etc.
        show-paren-mode                       ;; visible paren matching
        partial-completion-mode               ;; rapid M-x completions
        line-number-mode                      ;; show line no.
        column-number-mode                    ;; show char. no.
        ))

;; modes disabled by default
;; those commented are probably taken care of via X Resources for init speed
(mapc (lambda (x) (when (fboundp x) (funcall x -1)))
      '(
        menu-bar-mode                         ;; no menus, please
        tool-bar-mode                         ;; no toolbar, please
        scroll-bar-mode                       ;; no scrollbars, please
        ))

;; re-enable useful functions
(mapc (lambda (x) (put x 'disabled nil))
      '(
        downcase-region
	upcase-region
	scroll-left
        ))

;; global keys to make Emacs use the same key strokes as readline
(defun kill-line-reverse ()
  (interactive)
  (kill-line 0))
(global-set-key (kbd "C-u") 'kill-line-reverse)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c l") 'org-store-link)

; if you want negative argument, use M-- instead, same as readline

;; kill the bell/beep/ding/ring sound
(setq ring-bell-function (lambda ()))

;; password caching is not safe!
;; everything should be gpg-agent, ssh-agent, sudo time stamp
;;
(setq password-cache nil)

;; some identity stuff; nothing private
(setq user-full-name "effecting")
(setq user-mail-address "contact@effecting.us")

(provide 'conf-emacs)
