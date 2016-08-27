;;; user-variable.el -*- lexical-binding: t -*-

;;;;; Group
(defgroup helm-yplistener nil
  "yellow ppage viewer with helm interface"
  :group 'helm)

;;;;; Custom
(defcustom helm-yplistener-yp-urls
  '((sp  "bayonet.ddo.jp/sp")
    (tp  "temp.orz.hm/yp")
    ;; (dp  "dp.prgrssv.net")
    (hktv "games.himitsukichi.com/hktv")
    (turf-page "peercast.takami98.net/turf-page")
    ;; (oekaki "oekakiyp.appspot.com")
    (event "eventyp.xrea.jp")
    (message "peercast.takami98.net/message-yp"))
  "Yellow Page urls"
  :type 'list
  :group 'helm-yplistener)

(defcustom helm-yplistener-local-address
  '(:host "localhost"
    :port 7144)
  "local PeerCast addr:port"
  :type 'string
  :group 'helm-yplistener)

(defcustom helm-yplistener-player-type
  'mplayer2
  "player type"
  :type 'symbol
  :group 'helm-yplistener)

(defcustom helm-yplistener-bookmark-file-name
  "bookmarks.el"
  "file name for bookmark"
  :type "string"
  :group 'helm-yplistener)

(defcustom helm-yplistener-bookmark-directory-name
  "helm-yplistener"
  "file name for bookmark"
  :type "string"
  :group 'helm-yplistener)

(defcustom helm-yplistener-default-protocol
  "mmshttp"
  "default protocol for stream url"
  :type "string"
  :group 'helm-yplistener)

;;; Provide
(provide 'helm-yplistener-user-variable)
