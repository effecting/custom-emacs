;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; A custom configuration for loading EMMS just the way I want
;; and connecting to the Music Player Daemon.

(require 'emms)

(unless (equal emms-version "2.1")
  (error "Wrong EMMS version: %s" emms-version))

;; load emms
(defun my-emms-setup ()
  "custom load my emms settings for MPD"
  (interactive)
  ;; run mpd
  (require 'my-misc)
  (mpd-start)

  ;; some of the standard EMMS items
  (require 'emms)
  (require 'emms-playlist-mode)
  (require 'emms-info)
  (require 'emms-info-mp3info)
  (require 'emms-info-ogginfo)
  (require 'emms-cache)
  (require 'emms-browser)
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
  (add-to-list 'emms-info-functions 'emms-info-mp3info)
  (add-to-list 'emms-info-functions 'emms-info-ogginfo)
  (setq emms-track-description-function 'emms-info-track-description)
  (when (fboundp 'emms-cache) (emms-cache 1))
  
  ;; MPD setup
  (require 'emms-player-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (setq emms-player-mpd-verbose t)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "/Volumes/i_big/audio")
  (setq emms-player-mpd-sync-playlist t)

  ;; miscellanea
  (require 'emms-mode-line)
  (require 'emms-playing-time)
  (emms-mode-line 0) ; not good with multi-term emacs
  (emms-playing-time 1)
  (emms-playing-time-enable)

  ;; connect
  (emms-player-mpd-connect)
  ;; dump cache
  (emms-cache-set-from-mpd-all)
  (emms-cache-save)

  
  )

;; volume control
(global-set-key (kbd "<s-up>")
                '(lambda ()
                   (interactive)
                   (emms-volume-mpd-raise)
                   (message "MPD Volume raised")))
(global-set-key (kbd "<s-down>")
                '(lambda ()
                   (interactive)
                   (emms-volume-mpd-lower)
                   (message "MPD Volume lowered")))

;; some mpd controls
(global-set-key (kbd "s-e u") 'emms-player-mpd-update-all)
(global-set-key (kbd "s-e s") 'emms-player-mpd-show)
(global-set-key (kbd "s-e n") 'emms-player-mpd-next)
(global-set-key (kbd "s-e p") 'emms-player-mpd-previous)
(global-set-key (kbd "s-e d c") 'emms-player-mpd-connect)
(global-set-key (kbd "s-e d d") 'emms-player-mpd-disconnect)
(global-set-key (kbd "s-e l") 'emms-playlist-mode-go)
(global-set-key (kbd "s-e m") 'emms-playing-time-toggle)
;; keybindings to imitate Amarok super-key
(global-set-key (kbd "s-e v") 'emms-stop)
(global-set-key (kbd "s-e c") 'emms-pause)
(global-set-key (kbd "s-e x") 'emms-start)
(global-set-key (kbd "s-e z") 'emms-previous)
(global-set-key (kbd "s-e b") 'emms-next)

;; visual feedback is nice
(add-hook 'emms-player-paused-hook
          '(lambda ()
             (if emms-player-paused-p
                 (message "EMMS playback paused")
               (message "EMMS playback resumed"))))

(add-hook 'emms-player-started-hook
          '(lambda ()
             (message "EMMS playback started")))

(add-hook 'emms-player-stopped-hook
          '(lambda ()
             (message "EMMS playback stopped")))


(defun my-emms-setup-auto ()
  (interactive)
  (when (not (boundp 'my-emms-setup-auto-loaded))
    (setq my-emms-setup-auto-loaded t)
    (my-emms-setup)
    ;; change key binding to browsing now that we're all loaded
    (global-set-key (kbd "s-e s-e") 'my-emms-browse)
    ))
  
(defun my-emms-browse ()
  "custom browsing of MPD tracks"
  (interactive)
  ;; (emms-cache-set-from-mpd-all)
  (emms-browse-by-album)
  )

(provide 'set-emms-21)
