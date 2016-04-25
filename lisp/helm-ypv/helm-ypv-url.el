;;; url -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'glof)

(cl-defun helm-ypv-make-url (thing)
  (pcase thing
    (`[:bookmark ,b]
      (helm-ypv-make-url-bookmark b))
    (`[:channel ,c]
      (helm-ypv-make-url-channel c))))


(cl-defun helm-ypv-make-url-channel (channel)
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
                      helm-ypv-default-protocol)
                    helm-ypv-local-address
                    id type tracker)))

(cl-defun helm-ypv-make-url-bookmark (bkm)
  (with-slots (id tracker type) bkm
    (format "%s://%s/stream/%s.%s?tip=%s"
            (if (string-match-p (rx (or "flv"
                                       "FLV"
                                       "MKV"
                                       "mkv"))
                                type)
                "http"
              helm-ypv-default-protocol)
            helm-ypv-local-address
            id type tracker)))

(provide 'helm-ypv-url)

;;; url.el ends here
