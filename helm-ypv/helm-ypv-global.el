
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
    (dp  "dp.prgrssv.net")
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
  (cl-concatenate 'string "http://" info "/" "index.txt"))

(cl-defun helm-ypv-player (player url)
  (cl-case player
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
         (helm-ypv-info->channel
          (append (list yp-name)
                  (split-string x "<>"))))
     channels)))

(cl-defun helm-ypv-info->channel (info)
  (ypv-channel-new
   :yp (symbol-name (cl-first info))
   :name (cl-second info)
   :id (cl-third info)
   :ip (cl-fourth info)
   :url (cl-fifth info)
   :genre (cl-sixth info)
   :desc (cl-seventh info)
   :bitrate (cl-tenth info)
   :type (cl-nth-value 10 info)
   :time (cl-nth-value 16 info)
   :comment (cl-nth-value 18 info)
   ))

(cl-defun helm-ypv-replace-html-entities (str)
  (cl-letf ((ents '(("&lt;" "<")
                    ("&gt;" ">"))))
    (helm-ypv-replace-html-entites-internal
     ents str)))

(cl-defun helm-ypv-replace-html-entites-internal (lst str)
  (if (null lst)
      str
    (cl-letf* ((rep (car lst))
               (rep-str (replace-regexp-in-string
                         (car rep) (cadr rep)
                         str)))
      (helm-ypv-replace-html-entites-internal
       (cdr lst) rep-str))))


(cl-defun helm-ypv-url-retrieve (url)
  (cl-letf ((res (-> url
                   url-retrieve-synchronously
                   helm-ypv-remove-http-header)))
    (if (helm-ypv-empty-response-p res)
        nil
      (helm-ypv-replace-html-entities res))))


(cl-defun helm-ypv-get-channel (info)
  (cl-letf ((res (helm-ypv-url-retrieve
                  (helm-ypv-make-yp-index-url (cadr info)))))
    (if res
        (list (car info) res)
      nil)))

(cl-defun helm-ypv-get-channels (yp-info)
  (-map
   #'helm-ypv-get-channel
   yp-info))

(cl-defun helm-ypv-remove-http-header (buf)
  ;; remove header info, [[frozenlock.org/2012/07/07/url-retrieve-and-json-api]]
  (cl-letf ((content nil))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (setq content (buffer-substring-no-properties (+ 1 (point)) (- (point-max) 1)))
        (kill-buffer (current-buffer))))
    (helm-ypv-string->utf-8 content)))

(cl-defun helm-ypv-empty-response-p (str)
  (s-match "^\n$" str))

(cl-defun helm-ypv-string->utf-8 (str)
  (decode-coding-string str 'utf-8-unix))

(cl-defun helm-ypv-add-face (str face)
  (propertize str 'face face))

(cl-defun helm-ypv-get/parse-channels (yp-infos)
  (cl-mapcan
   #'helm-ypv-parse-channels
   (cl-remove nil (helm-ypv-get-channels yp-infos))))

(provide 'helm-ypv-global)
