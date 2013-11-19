
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)
(require 'dash)
(require 's)

(defgroup helm-ypv nil
  "yellow ppage viewer with helm interface"
  :group 'helm)

(defcustom helm-ypv-yp-urls
  '((sp  "bayonet.ddo.jp/sp")
    (tp  "temp.orz.hm/yp")
    ;; (dp  "dp.prgrssv.net")
    (hktv "games.himitsukichi.com/hktv")
    (turf-page "peercast.takami98.net/turf-page")
    (oekaki "oekakiyp.appspot.com"))
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


(cl-defun helm-ypv-make-yp-index-url (info)
  (concat "http://" info "/" "index.txt"))

(cl-defun helm-ypv-player (player url)
  (case player
    (mplayer2
     (helm-ypv-player-mplayer2 url))))

(cl-defun helm-ypv-player-mplayer2 (url)
  (message url)
  (cl-letf ((command (concat "mplayer --playlist="
                             "'" url "'"
                             " --softvol --nocache --framedrop --really-quiet --no-consolecontrols"
                             " &" )))
    (message command)
    (start-process-shell-command "ypv" nil command)))

(cl-defun helm-ypv-parse-channels (info)
  (cl-letf* ((yp-name (car info))
             (content (cadr info))
             (channels (split-string content "\n")))
    (-map
     #'(lambda (x)
         (helm-ypv-info-to-channel
          (append (list yp-name)
                  (split-string x "<>"))))
     channels)))


(provide 'helm-ypv-global)
