;;; helm-ypv.el --- yp viewer with helm

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)

(defvar ypv-yp-urls
  '((sp  "bayonet.ddo.jp/sp")
    (tp  "temp.orz.hm/yp")
    (dp  "dp.prgrssv.net"))
  "Yellow Pages urls")

(defvar ypv-local-address
  "localhost:7155"
  "local PeerCast addr:port")

(cl-defun ypv--make-yp-index-url (info)
  (concat "http://" info "/" "index.txt"))

(cl-defun ypv--url-retrieve (url)
  (ypv--replace-html-entities
   (ypv--remove-http-header
    (url-retrieve-synchronously url))))

(cl-defun ypv--get-channels (yp-info)
  (cl-mapcar
   #'(lambda (info)
       `(,(car info)
         ,(ypv--url-retrieve
           (ypv--make-yp-index-url (cadr info)))))
   yp-info))

(cl-defun ypv--remove-http-header (buf)
  ;; remove header info [[frozenlock.org/2012/07/07/url-retrieve-and-json-api]]
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
    (cl-mapcar
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
   :desc (cl-seventh info)))

(cl-defun ypv--replace-html-entities (str)
  (cl-letf ((ents '(("&lt;" "<")
                    ("&gt;" ">"))))
    (ypv--replace-html-entites-internal
     ents
     str)))

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
                          (plist-get info :id) ; id
                          (plist-get info :ip) ; ip
                          )))
    (ypv-player-mplayer url)))

(cl-defun ypv-player-mplayer (url)
  (message url)
  (cl-letf ((command (concat "mplayer --playlist="
                             "'" url "'"
                             " --softvol --nocache --framedrop --no-consolecontrols"
                             )))
    (message command)
    (start-process-shell-command "ypv" nil command)))


(cl-defun ypv-create-candidates ()
  (cl-mapcar
   #'(lambda (info)
       (cons
        ;; display
        (ypv-create-display-candidate info)
        ;; real
        info))
   (ypv--get/parse-channels ypv-yp-urls)))

(cl-defun ypv-create-display-candidate (info)
  (format "%s %s %s"
          (propertize (plist-get info :name) 'face 'font-lock-type-face)
          (plist-get info :desc)
          (plist-get info :url)))

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
