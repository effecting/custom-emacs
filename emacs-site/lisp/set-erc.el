;; 
;; 

;; Authored by effecting2 in 2006-2007,2011.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'erc)
(require 'erc-networks)
(require 'erc-services)
(erc-services-mode 1)

;; security
(setq erc-disable-ctcp-replies t)

;; modules
(setq erc-modules '(
                    autoaway
                    autojoin
                    button
                    completion
                    fill
                    irccontrols
                    log
                    match
                    netsplit
                    noncommands
                    notify
                    pcomplete
                    readonly
                    ring
                    services
                    smiley
                    spelling
                    stamp
                    track
                    ) )

;; nicks, patterns, and the like
;; (setq erc-away-nickname "effecting|away")
(setq erc-away-nickname nil)
(setq erc-nick "effecting")
(setq erc-prompt-for-nickserv-password nil)
(setq erc-prompt-for-password nil)
(setq erc-user-full-name "http://effecting.us/")
(setq erc-notify-interval 120)
(setq erc-notify-signon-hook '(erc-notify-signon))
; (setq erc-current-nick-highlight-type 'keyword)
(setq erc-current-nick-highlight-type nil)
(setq erc-keyword-highlight-type 'keyword)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-log-matches-flag t)
(setq erc-text-matched-hook '(erc-log-matches erc-beep-on-match))
(setq erc-format-query-as-channel-p nil)
(setq erc-track-exclude-server-buffer t)
(setq erc-track-position-in-mode-line 'before-modes)

;; networks
(setq erc-port 6667)
(setq erc-networks-alist '(
                           (DALnet "dal.net")
                           (ICQnet "icq.com")
                           (OFTC "oftc.net")
                           (Undernet "undernet.org")
                           (freenode "freenode.net")
                           (EFFECTING "effecting.us")
                           (LOCALHOST "127.0.0.1")
                           (GIMPnet "gimp.org")
                           (GNOME "gnome.org")
                           ) )

(setq erc-server-alist '(
                         ("Bitlbee P" LOCALHOST "localhost" 6667)
                         ("Bitlbee K" EFFECTING "fake.effecting.us" 6667)
                         ("DALnet: Random US server" DALnet "irc.dal.net" ((6660 6667)))
                         ("DALnet: US, GA, Astro" DALnet "astro.ga.us.dal.net" ((6661 6669) 7000))
                         ("DALnet: US, GA, Dragons" DALnet "dragons.ga.us.dal.net" ((6661 6669) 7000))
                         ("DALnet: US, GA, Elysium" DALnet "elysium.ga.us.dal.net" ((6661 6669) 7000))
                         ("DALnet: US, MA, Twisted" DALnet "twisted.ma.us.dal.net" ((6660 6669) 7001 7002))
                         ("DALnet: US, MO, Global" DALnet "global.mo.us.dal.net" ((6661 6669) 7000))
                         ("DALnet: US, NJ, Liberty" DALnet "liberty.nj.us.dal.net" ((6662 6669) 7000))
                         ("DALnet: US, VA, Wombat" DALnet "wombat.va.us.dal.net" ((6661 6669) 7000))
                         ("Freenode: Random US server" freenode "irc.us.freenode.net" 6667)
                         ("ICQnet: Random server" ICQnet "irc.icq.com" 6667)
                         ("GIMPnet" GIMPnet "irc.gimp.org" 6667)
                         ("GNOME" GNOME "irc.gnome.org" 6667)
                         ("OFTC: Random server" OFTC "irc.oftc.net" 6667)
                         ("Undernet: Random US server" Undernet "us.undernet.org" 6667)
                         ("Undernet: US, AZ, Mesa" Undernet "mesa.az.us.undernet.org" ((6665 6667)))
                         ("Undernet: US, CA, San Diego" Undernet "sandiego.ca.us.undernet.org" ((6660 6670)))
                         ("Undernet: US, DC, Washington" Undernet "washington.dc.us.undernet.org" ((6660 6669)))
                         ("Undernet: US, KS, Manhattan" Undernet "manhattan.ks.us.undernet.org" ((6660 6669)))
                         ("Undernet: US, NV, Las Vegas" Undernet "lasvegas.nv.us.undernet.org" ((6660 6669)))
                         ("Undernet: US, TX, Austin" Undernet "austin.tx.us.undernet.org" ((6660 6669)))
                         ("Undernet: US, UT, Saltlake" Undernet "saltlake.ut.us.undernet.org" ((6660 6669)))
                         ("Undernet: US, VA, Arlington" Undernet "arlington.va.us.undernet.org" ((6660 6669)))
                         ("Undernet: US, VA, McLean" Undernet "mclean.va.us.undernet.org" ((6666 6669)))
                         ) )

(setq erc-nickserv-alist '(
                           (DALnet "NickServ!service@dal.net" "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>" "NickServ@services.dal.net" "IDENTIFY" nil)
                           (freenode "NickServ!NickServ@services." "/msg\\s-NickServ\\s-identify\\s-<password>" "NickServ" "IDENTIFY" nil)
                           (OFTC "NickServ!services@services.oftc.net" "/msg\\s-NickServ\\s-IDENTIFY\\s-password" "NickServ" "IDENTIFY" nil)
                           ) )

;; some behavior
(setq erc-autoaway-idle-method 'user)

;; modified form of erc-server-select, automatically use first server in alist for network name
(defun erc-network-select ()
  "Interactively select a network to connect to using `erc-networks-alist'."
  (interactive)
  (require 'erc)
  (let* ( (completion-ignore-case t)
          (net (intern
                (completing-read "Network: "
                                 (erc-delete-dups
                                  (mapcar (lambda (x)
                                            (list (symbol-name (nth 1 x))))
                                          erc-server-alist)))))
          (srv (car (delq nil (mapcar (lambda (x)
                                        (if (equal (nth 1 x) net) x))
                                      erc-server-alist))))
          (host (nth 2 srv))
          (ports (if (listp (nth 3 srv))
                     (erc-ports-list (nth 3 srv))
                   (list (nth 3 srv))))
          (port (nth (random (length ports)) ports))
          )
    (if (equal net 'EFFECTING)
        (erc-effecting)
      (if (equal net 'LOCALHOST)
          (erc-localhost)
        (erc :server host :port port)
        )
      )))

;; logging
(setq erc-log-channels-directory "~/.emacs.d/erc-logs/")
(setq erc-save-buffer-on-part t)
(setq erc-log-write-after-send t)
(setq erc-save-queries-on-quit t)

;; use ctrl-u as kill-line in erc prompt
(defadvice erc-mode (after my-erc-mode activate)
  (local-set-key (kbd "C-u") 'erc-kill-input))

;; non-standard functionality
(autoload 'erc-nicklist "erc-nicklist" nil t)
;; no icons
(setq erc-nicklist-use-icons nil)

;; play sounds
(add-hook 'erc-text-matched-hook 'erc-sound-if-not-server)
(defun erc-sound-if-not-server (match-type nickuserhost msg)
  (unless (string-match "Server:[0-9]+" nickuserhost)
    (start-process-shell-command "*erc sound*" nil "mplayer /usr/share/skype/sounds/ChatIncoming.wav")))

;; private passwords in custom.el
(provide 'set-erc)
