;;; helm-ypv.el --- yp viewer with helm

;;; Commentary:

;;; Code:

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
  'mplayer
  "player type"
  :type 'symbol
  :group 'helm-ypv)

(cl-defun helm-ypv-make-yp-index-url (info)
  (concat "http://" info "/" "index.txt"))

(cl-defun helm-ypv-url-retrieve (url)
  (-> url
    url-retrieve-synchronously
    helm-ypv-remove-http-header
    helm-ypv-replace-html-entities))

(cl-defun helm-ypv-get-channel (info)
  (list (car info)
        (helm-ypv-url-retrieve
         (helm-ypv-make-yp-index-url (cadr info)))))

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

(cl-defun helm-ypv-string->utf-8 (str)
  (decode-coding-string str 'utf-8-unix))

(cl-defun helm-ypv-parse-channels (info)
  (cl-letf* ((yp-name (car info))
             (content (cadr info))
             (channels (split-string content "\n")))
    (-map
     #'(lambda (x)
         (helm-ypv-channel-info-to-plist
          (append (list yp-name)
                  (split-string x "<>"))))
     channels)))

(cl-defun helm-ypv-channel-info-to-plist (info)
  ;; name<>id<>ip:port<>url<>genre<>desc<>169<>163<>bitrate<>type<><><><><><>time<><>comment<>0
  (list
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
   :comment (cl-nth-value 18 info)))

(cl-defun helm-ypv-channel-info-name (info)
  (plist-get info :name))
(cl-defun helm-ypv-channel-info-id (info)
  (plist-get info :id))
(cl-defun helm-ypv-channel-info-desc (info)
  (plist-get info :desc))
(cl-defun helm-ypv-channel-info-url (info)
  (plist-get info :url))
(cl-defun helm-ypv-channel-info-type (info)
  (plist-get info :type))
(cl-defun helm-ypv-channel-info-ip (info)
  (plist-get info :ip))
(cl-defun helm-ypv-channel-info-genre (info)
  (plist-get info :genre))
(cl-defun helm-ypv-channel-info-bitrate (info)
  (plist-get info :bitrate))
(cl-defun helm-ypv-channel-info-time (info)
  (plist-get info :time))
(cl-defun helm-ypv-channel-info-comment (info)
  (plist-get info :comment))

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

(cl-defun helm-ypv-get/parse-channels (yp-infos)
  (cl-mapcan
   #'helm-ypv-parse-channels
   (helm-ypv-get-channels yp-infos)))

(cl-defun helm-ypv-action-open-url (candidate)
  (cl-letf* ((info candidate)
             (url (helm-ypv-make-url info)))
    (helm-ypv-player helm-ypv-player-type url)))

(cl-defun helm-ypv-player (player url)
  (case player
    (mplayer
     (helm-ypv-player-mplayer url))))

(cl-defun helm-ypv-player-mplayer (url)
  (message url)
  (cl-letf ((command (concat "mplayer --playlist="
                             "'" url "'"
                             " --softvol --nocache --framedrop --really-quiet --no-consolecontrols"
                             " &" )))
    (message command)
    (start-process-shell-command "ypv" nil command)))

(cl-defun helm-ypv-make-url (info)
  (format "http://%s/pls/%s?tip=%s"
          helm-ypv-local-address
          (helm-ypv-channel-info-id info)
          (helm-ypv-channel-info-ip info)))

(cl-defun helm-ypv-create-candidates ()
  (-map
   #'(lambda (info)
       (cons
        ;; display candidate
        (helm-ypv-create-display-candidate info)
        ;; real candidate
        info))
   (helm-ypv-get/parse-channels helm-ypv-yp-urls)))

(cl-defun helm-ypv-add-face (str face)
  (propertize str 'face face))

(cl-defun helm-ypv-create-display-candidate (info)
  (cl-letf ((format-string "%-17.17s %s [%s] %s %s %s %s")
            (name (helm-ypv-add-face (helm-ypv-channel-info-name info) 'font-lock-type-face))
            (genre (helm-ypv-add-face (helm-ypv-channel-info-genre info) 'font-lock-keyword-face))
            (desc (helm-ypv-add-face (helm-ypv-channel-info-desc info) 'font-lock-string-face))
            (url (helm-ypv-add-face (helm-ypv-channel-info-url info) 'font-lock-reference-face))
            (type (helm-ypv-add-face (helm-ypv-channel-info-type info) 'font-lock-type-face))
            (bitrate (helm-ypv-add-face (helm-ypv-channel-info-bitrate info) 'font-lock-preprocessor-face))
            (time (helm-ypv-add-face (helm-ypv-channel-info-time info) 'font-lock-preprocessor-face))
            (comment (helm-ypv-add-face (helm-ypv-channel-info-comment info) 'font-lock-preprocessor-face)))
    (format format-string
            name
            desc
            comment
            genre
            url
            type
            time)))

(cl-defun helm-ypv-create-sources ()
  `((name . "channel list")
    (candidates . ,(helm-ypv-create-candidates))
    (action . (("Open url" .  helm-ypv-action-open-url)))))


;;;###autoload
(cl-defun helm-ypv ()
  "Yellow Page viwer with Helm"
  (interactive)
  (helm (helm-ypv-create-sources)))

(provide 'helm-ypv)

;;; helm-ypv.el ends here
