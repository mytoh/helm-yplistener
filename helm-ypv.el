;;; helm-ypv.el --- yp viewer with helm

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)
(require 'dash)
(require 's)


(defvar ypv-yp-urls
  '((sp  "bayonet.ddo.jp/sp")
    (tp  "temp.orz.hm/yp")
    (dp  "dp.prgrssv.net")
    )
  "Yellow Pages urls")

(defvar ypv-local-address
  "localhost:7155"
  "local PeerCast addr:port")

(cl-defun ypv--make-yp-index-url (info)
  (concat "http://" info "/" "index.txt"))

(cl-defun ypv--url-retrieve (url)
  (-> url
    url-retrieve-synchronously
    ypv--remove-http-header
    ypv--replace-html-entities))

(cl-defun ypv--get-channel (info)
  (list (car info)
        (ypv--url-retrieve
         (ypv--make-yp-index-url (cadr info)))))

(cl-defun ypv--get-channels (yp-info)
  (-map
   #'ypv--get-channel
   yp-info))

(cl-defun ypv--remove-http-header (buf)
  ;; remove header info, [[frozenlock.org/2012/07/07/url-retrieve-and-json-api]]
  (cl-letf ((content nil))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (setq content (buffer-substring-no-properties (+ 1 (point)) (- (point-max) 1)))
        (kill-buffer (current-buffer))))
    (ypv--string->utf-8 content)))

(cl-defun ypv--string->utf-8 (str)
  (decode-coding-string str 'utf-8-unix))

(cl-defun ypv--parse-channels (info)
  (cl-letf* ((yp-name (car info))
             (content (cadr info))
             (channels (split-string content "\n")))
    (-map
     #'(lambda (x)
         (ypv--channel-info-to-plist
          (append (list yp-name)
                  (split-string x "<>"))))
     channels)))

(cl-defun ypv--channel-info-to-plist (info)
  (list
   :yp (symbol-name (cl-first info))
   :name (cl-second info)
   :id (cl-third info)
   :ip (cl-fourth info)
   :url (cl-fifth info)
   :genre (cl-sixth info)
   :desc (cl-seventh info)
   :type (cl-nth-value 10 info)))

(cl-defun ypv--channel-info-name (info)
  (plist-get info :name))
(cl-defun ypv--channel-info-id (info)
  (plist-get info :id))
(cl-defun ypv--channel-info-desc (info)
  (plist-get info :desc))
(cl-defun ypv--channel-info-url (info)
  (plist-get info :url))
(cl-defun ypv--channel-info-type (info)
  (plist-get info :type))
(cl-defun ypv--channel-info-ip (info)
  (plist-get info :ip))
(cl-defun ypv--channel-info-genre (info)
  (plist-get info :genre))


(cl-defun ypv--replace-html-entities (str)
  (cl-letf ((ents '(("&lt;" "<")
                    ("&gt;" ">"))))
    (ypv--replace-html-entites-internal
     ents str)))

(cl-defun ypv--replace-html-entites-internal (lst str)
  (if (null lst)
      str
    (cl-letf* ((rep (car lst))
               (rep-str (replace-regexp-in-string
                         (car rep) (cadr rep)
                         str)))
      (ypv--replace-html-entites-internal
       (cdr lst) rep-str))))


(cl-defun ypv--get/parse-channels (yp-infos)
  (cl-mapcan
   #'ypv--parse-channels
   (ypv--get-channels yp-infos)))

(cl-defun ypv-action-open-url (candidate)
  (cl-letf* ((info candidate)
             (url (format "http://%s/pls/%s?tip=%s"
                          ypv-local-address
                          (ypv--channel-info-id info)
                          (ypv--channel-info-ip info))))
    (ypv-player-mplayer url)))

(cl-defun ypv-player-mplayer (url)
  (message url)
  (cl-letf ((command (concat "mplayer --playlist="
                             "'" url "'"
                             " --softvol --nocache --framedrop --no-consolecontrols"
                             " &" )))
    (message command)
    (start-process-shell-command "ypv" nil command)))


(cl-defun ypv-create-candidates ()
  (-map
   #'(lambda (info)
       (cons
        ;; display candidate
        (ypv-create-display-candidate info)
        ;; real candidate
        info))
   (ypv--get/parse-channels ypv-yp-urls)))

(cl-defun ypv-create-display-candidate (info)
  (format "%s %s %s %s %s"
          (s-pad-right 15 " "
                       (propertize (ypv--channel-info-name info) 'face 'font-lock-type-face))
          (ypv--channel-info-desc info)
          (ypv--channel-info-genre info)
          (ypv--channel-info-url info)
          (ypv--channel-info-type info)))

(cl-defun ypv-create-sources ()
  `((name . "channel list")
    (candidates . ,(ypv-create-candidates))
    (action . (("Open url" .  ypv-action-open-url)))))


;;;###autoload
(cl-defun helm-ypv ()
  "Helm command for yp."
  (interactive)
  (helm (ypv-create-sources)))

(provide 'helm-ypv)

;;; helm-ypv.el ends here
