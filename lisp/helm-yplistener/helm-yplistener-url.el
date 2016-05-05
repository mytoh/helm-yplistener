;;; url -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'glof)

(cl-defun helm-yplistener-make-url (thing)
  (pcase thing
    (`[:bookmark ,b]
      (helm-yplistener-make-url-bookmark b))
    (`[:channel ,c]
      (helm-yplistener-make-url-channel c))))


(cl-defun helm-yplistener-make-url-channel (channel)
  (glof:let (((id :id)
              (tracker :tracker)
              (type :type))
             channel)
    (format "%s://%s/stream/%s.%s?tip=%s"
            (if (string-match-p (rx (or "flv"
                                       "FLV"
                                       "mkv"
                                       "MKV"))
                                type)
                "http"
              helm-yplistener-default-protocol)
            helm-yplistener-local-address
            id type tracker)))

(cl-defun helm-yplistener-make-url-bookmark (bkm)
  (glof:let (((id :id)
              (tracker :tracker)
              (type :type))
             bkm)
    (format "%s://%s/stream/%s.%s?tip=%s"
            (if (string-match-p (rx (or "flv"
                                       "FLV"
                                       "MKV"
                                       "mkv"))
                                type)
                "http"
              helm-yplistener-default-protocol)
            helm-yplistener-local-address
            id type tracker)))

(provide 'helm-yplistener-url)

;;; url.el ends here
