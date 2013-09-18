;;; helm-ypv.el --- yp viewer with helm

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)

(defvar ypv-yp-urls
  '((sp  "bayonet.ddo.jp/sp")
    (tp  "temp.orz.hm/yp")
    (dp  "dp.prgrssv.net"))
  "Yellow Pages urls")

(defvar ypv-local-address
  "localhost:7155"
  "local PeerCast addr:port")

(defun ypv--make-yp-index-url (info)
  (concat "http://" info "/" "index.txt"))

(defun ypv--url-retrieve (url)
  (ypv--replace-html-entities
   (ypv--remove-http-header
    (url-retrieve-synchronously url))))

(defun ypv--get-channels (yp-info)
  (cl-mapcar
   #'(lambda (info)
       `(,(car info)
         ,(ypv--url-retrieve
           (ypv--make-yp-index-url (cadr info)))))
   yp-info))

(defun ypv--remove-http-header (buf)
  ;; remove header info [[frozenlock.org/2012/07/07/url-retrieve-and-json-api]]
  (let ((content nil))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (setq content (buffer-substring-no-properties (+ 1 (point)) (- (point-max) 1)))
        (kill-buffer (current-buffer))))
    (ypv--string->utf-8 content)))

(defun ypv--string->utf-8 (str)
  (decode-coding-string str 'utf-8-unix))

(defun ypv--parse-channels (info)
  (let* ((yp-name (car info))
         (content (cadr info))
         (channels (split-string content "\n")))
    (cl-mapcar
     (lambda (x)
       (ypv--channel-info-to-plist
        (append (list yp-name)
                (split-string x "<>"))))
     channels)))

(defun ypv--channel-info-to-plist (info)
  (list
   :yp (symbol-name (nth 0 info))
   :name (nth 1 info)
   :id (nth 2 info)
   :ip (nth 3 info)
   :url (nth 4 info)
   :genre (nth 5 info)
   :desc (nth 6 info)))

(defun ypv--replace-html-entities (str)
  (let ((ents '(("&lt;" "<")
                ("&gt;" ">"))))
    (ypv--replace-html-entites-internal
     ents
     str)))

(defun ypv--replace-html-entites-internal (lst str)
  (if (null lst)
      str
    (let* ((rep (car lst))
           (rep-str (replace-regexp-in-string
                     (car rep) (cadr rep)
                     str)))
      (ypv--replace-html-entites-internal
       (cdr lst) rep-str))))


(defun ypv--get/parse-channels (yp-infos)
  (cl-mapcan
   #'ypv--parse-channels
   (ypv--get-channels yp-infos)))

(defun ypv-action-open-url (candidate)
  (let* ((info candidate)
         (url (format "http://%s/pls/%s?tip=%s"
                      ypv-local-address
                      (plist-get info :id) ; id
                      (plist-get info :ip) ; addr
                      )))
    (ypv-player-mplayer url)))


(defun ypv-player-mplayer (url)
  (message url)
  (let ((args
         (list
          "ypv"
          nil
          "mplayer" "-playlist" url
          "-nocache" "-framedrop" "-noconsolecontrols")))
    (apply #'start-process args)))

(defun ypv-create-candidates ()
  (cl-mapcar
   #'(lambda (info)
       (cons
        ;; display
        (ypv-create-display-candidate info)
        ;; real
        info))
   (ypv--get/parse-channels ypv-yp-urls)))

(defun ypv-create-display-candidate (info)
  (format "%s %s %s"
          (propertize (plist-get info :name) 'face 'font-lock-type-face)
          (plist-get info :desc)
          (plist-get info :url)))

(defun ypv-create-sources ()
  `((name . "channel list")
    (candidates . ,(ypv-create-candidates))
    (action . (("Open url" .  ypv-action-open-url)))))


;;;###autoload
(defun helm-ypv ()
  "Helm command for yp."
  (interactive)
  (helm (ypv-create-sources)))

(provide 'helm-ypv)

;;; helm-ypv.el ends here
