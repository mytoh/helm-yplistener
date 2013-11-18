;;; helm-ypv.el --- yp viewer with helm

;;; Commentary:

;;; Code:


;;;; deps
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)
(require 'dash)
(require 's)

;;;; helm-ypv
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
         (helm-ypv-info-to-channel
          (append (list yp-name)
                  (split-string x "<>"))))
     channels)))

(cl-defstruct (ypv-channel
               (:constructor ypv-channel-new))
  (yp "")
  (name "")
  (id "")
  (ip "")
  (url "")
  (genre "")
  (desc "")
  (bitrate "")
  (type "")
  (time "")
  (comment "")
  )

(cl-defun helm-ypv-info-to-channel (info)
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

(cl-defun helm-ypv-get/parse-channels (yp-infos)
  (cl-mapcan
   #'helm-ypv-parse-channels
   (helm-ypv-get-channels yp-infos)))

(cl-defun helm-ypv-action-open-channel (candidate)
  (cl-letf* ((info candidate)
             (url (helm-ypv-make-url info)))
    (helm-ypv-player helm-ypv-player-type url)))

(cl-defun helm-ypv-make-url (channel)
  (format "http://%s/pls/%s?tip=%s"
          helm-ypv-local-address
          (ypv-channel-id channel)
          (ypv-channel-ip channel)))

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


(cl-defun helm-ypv-create-candidates (channels)
  (-map
   #'(lambda (info)
       (cons
        ;; display candidate
        (helm-ypv-create-display-candidate info)
        ;; real candidate
        info))
   channels
   ))

(cl-defun helm-ypv-add-face (str face)
  (propertize str 'face face))

(cl-defun helm-ypv-create-display-candidate (channel)
  (cl-letf ((format-string "%-17.17s %s [%s] %s %s %s %s")
            (name (helm-ypv-add-face (ypv-channel-name channel) 'font-lock-type-face))
            (genre (helm-ypv-add-face (ypv-channel-genre channel) 'font-lock-keyword-face))
            (desc (helm-ypv-add-face (ypv-channel-desc channel) 'font-lock-string-face))
            (url (helm-ypv-add-face (ypv-channel-url channel) 'font-lock-reference-face))
            (type (helm-ypv-add-face (ypv-channel-type channel) 'font-lock-type-face))
            (bitrate (helm-ypv-add-face (ypv-channel-bitrate channel) 'font-lock-preprocessor-face))
            (time (helm-ypv-add-face (ypv-channel-time channel) 'font-lock-preprocessor-face))
            (comment (helm-ypv-add-face (ypv-channel-comment channel) 'font-lock-preprocessor-face)))
    (format format-string
            name
            desc
            comment
            genre
            url
            type
            time)))

(cl-defun helm-source-ypv-channels ()
  `((name . "Channel list")
    (candidates . ,(helm-ypv-create-candidates (helm-ypv-get/parse-channels helm-ypv-yp-urls)))
    (action . (("Open channel" .  helm-ypv-action-open-channel)
               ("Add to bookmarks" . helm-ypv-bookmark-action-add)))))

;;;###autoload
(cl-defun helm-ypv ()
  "Preconfigured `helm' for Yellow Pages"
  (interactive)
  (helm :sources (list
                  (helm-source-ypv-channels)
                  (helm-source-ypv-bookmarks)
                  )
        :buffer "*Helm ypv*"))


;;;; helm-ypv-bookmark

;;;;; internal

(cl-defun helm-ypv-bookmark-make-url (bkm)
  (format "http://%s/pls/%s?tip=%s"
          helm-ypv-local-address
          (ypv-bookmark-id bkm)
          (ypv-bookmark-ip bkm)))

(cl-defun helm-ypv-bookmark-compare-id (bmk1 bmk2)
  (equal (ypv-bookmark-id bmk1)
         (ypv-bookmark-id bmk2)))

;;;;; bookmark data file

(cl-defun helm-ypv-bookmark-data-write (file data)
  (with-temp-file file
    (cl-letf ((standard-output (current-buffer)))
      (prin1 data))))

(cl-defun helm-ypv-bookmark-data-add (file data)
  (if (file-exists-p file)
      (if (helm-ypv-bookmark-data-channel-exists-p file data)
          (helm-ypv-bookmark-data-update file data)
        (helm-ypv-bookmark-data-append file data))
    (helm-ypv-bookmark-data-write file (list data))))

(cl-defun helm-ypv-bookmark-data-append (file data)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "updating bookmark")
    (helm-ypv-bookmark-data-write file (append old (list data)))
    (message (format "added %s" data))))

(cl-defun helm-ypv-bookmark-data-update (file data)
  (cl-letf ((old (cl-remove-if
                  (lambda (old-bookmark)
                    (helm-ypv-bookmark-compare-id
                     old-bookmark data))
                  (helm-ypv-bookmark-data-read file))))
    (message "updating bookmark")
    (helm-ypv-bookmark-data-write file (append old (list data)))
    (message (format "update to add %s" data))))

(cl-defun helm-ypv-bookmark-data-remove (file bookmark)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "removing bookmark")
    (helm-ypv-bookmark-data-write file
                                  (cl-remove-if
                                   (lambda (old-bookmark)
                                     (helm-ypv-bookmark-compare-id
                                      old-bookmark bookmark))
                                   old))
    (message (format "removed %s" bookmark))))

(cl-defun helm-ypv-bookmark-data-channel-exists-p (file bkm)
  (cl-find bkm (helm-ypv-bookmark-data-read file)
           :test #'equal))

(cl-defun helm-ypv-bookmark-data-read (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(cl-defun helm-ypv-bookmark-data-file ()
  (expand-file-name "helm-ypv-bookmarks" user-emacs-directory))

;;;;; bookmark info

(cl-defstruct (ypv-bookmark
               (:constructor ypv-bookmark-new))
  (yp "")
  (name "")
  (id "")
  (ip "")
  (url "")
  (genre "")
  (desc "")
  (bitrate "")
  (type "")
  (time "")
  (comment "")
  (broadcasting nil))

(cl-defun helm-ypv-bookmark-channel->bookmark (channel)
  (ypv-bookmark-new
   :yp (ypv-channel-yp channel)
   :name (ypv-channel-name channel)
   :id (ypv-channel-id channel)
   :ip (ypv-channel-ip channel)
   :url (ypv-channel-url channel)
   :genre (ypv-channel-genre channel)
   :desc (ypv-channel-desc channel)
   :bitrate (ypv-channel-bitrate channel)
   :type (ypv-channel-type channel)
   :time (ypv-channel-time  channel)
   :comment (ypv-channel-comment channel)
   :broadcasting nil))


;;;;; action

(cl-defun helm-ypv-bookmark-action-add (candidate)
  (cl-letf* ((info (helm-ypv-bookmark-channel->bookmark candidate)))
    (helm-ypv-bookmark-data-add (helm-ypv-bookmark-data-file) info)))

(cl-defun helm-ypv-bookmark-action-remove (candidate)
  (cl-letf* ((info candidate))
    (helm-ypv-bookmark-data-remove (helm-ypv-bookmark-data-file) info)))

(cl-defun helm-ypv-bookmark-action-open-channel (candidate)
  (cl-letf* ((bookmark candidate)
             (url (helm-ypv-bookmark-make-url bookmark)))
    (helm-ypv-bookmark-data-update (helm-ypv-bookmark-data-file) bookmark)
    (helm-ypv-player helm-ypv-player-type url)))

;;;;; candidate

(cl-defun helm-ypv-bookmark-create-display-candidate (bookmark)
  (cl-letf ((format-string "%-17.17s %s")
            (name (helm-ypv-add-face (ypv-bookmark-name bookmark) (if (ypv-bookmark-broadcasting bookmark)
                                                                      'font-lock-type-face
                                                                    'font-lock-dcc-face)))
            (id (helm-ypv-add-face (ypv-bookmark-id bookmark) 'font-lock-preprocessor-face)))
    (format format-string
            name
            id)))

(cl-defun helm-ypv-bookmark-channel-broadcasting-p (bookmark channels)
  (cl-find-if #'(lambda (channel)
                  (equal (ypv-bookmark-id bookmark)
                         (ypv-channel-id channel)))
              channels))

(cl-defun helm-ypv-bookmark-find-broadcasting-channels (bookmarks channels)
  (cl-remove-if
   #'(lambda (b) (eq b nil))
   (cl-map 'list
           #'(lambda (bookmark)
               (if (helm-ypv-bookmark-channel-broadcasting-p bookmark channels)
                   (helm-ypv-bookmark-set-broadcasting bookmark t)
                 (helm-ypv-bookmark-set-broadcasting bookmark nil)))
           bookmarks)))

(cl-defun helm-ypv-bookmark-set-broadcasting (obj value)
  (setf (ypv-bookmark-broadcasting obj) value)
  obj)

(cl-defun helm-ypv-bookmark-create-candidates (channels)
  (if (not (file-exists-p (helm-ypv-bookmark-data-file)))
      '()
    (-map
     #'(lambda (bookmark)
         (cons
          ;; display candidate
          (helm-ypv-bookmark-create-display-candidate bookmark)
          ;; real candidate
          bookmark))
     (helm-ypv-bookmark-find-broadcasting-channels
      (helm-ypv-bookmark-data-read (helm-ypv-bookmark-data-file))
      channels))))

;;;;; source

(cl-defun helm-source-ypv-bookmarks ()
  `((name . "Bookmarks")
    (candidates . ,(helm-ypv-bookmark-create-candidates (helm-ypv-get/parse-channels helm-ypv-yp-urls)))
    (action . (("Open channel" . helm-ypv-bookmark-action-open-channel)
               ("Remove bookmark" . helm-ypv-bookmark-action-remove)))))

;;;###autoload
(cl-defun helm-ypv-bookmarks ()
  "Preconfigured `helm' for Yellow Pages bookmarks"
  (interactive)
  (helm :sources (list
                  (helm-source-ypv-bookmarks))
        :buffer "*Helm ypv bookmarks*"))






;;;; provide
(provide 'helm-ypv)

;;; helm-ypv.el ends here
