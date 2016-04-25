;;; user-variable.el -*- lexical-binding: t -*-

;;;;; Group
(defgroup helm-ypv nil
  "yellow ppage viewer with helm interface"
  :group 'helm)

;;;;; Custom
(defcustom helm-ypv-yp-urls
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
  :group 'helm-ypv)

(defcustom helm-ypv-local-address
  "localhost:7144"
  "local PeerCast addr:port"
  :type 'string
  :group 'helm-ypv)

(defcustom helm-ypv-player-type
  'mplayer2
  "player type"
  :type 'symbol
  :group 'helm-ypv)

(defcustom helm-ypv-bookmark-file-name
  "bookmarks.el"
  "file name for bookmark"
  :type "string"
  :group 'helm-ypv)

(defcustom helm-ypv-bookmark-directory-name
  "helm-ypv"
  "file name for bookmark"
  :type "string"
  :group 'helm-ypv)

(defcustom helm-ypv-default-protocol
  "mmshttp"
  "default protocol for stream url"
  :type "string"
  :group 'helm-ypv)

;;; Provide
(provide 'helm-ypv-user-variable)
