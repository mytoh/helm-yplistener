;;; url -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(cl-defmethod helm-ypv-make-url ((channel ypv-channel))
  (with-slots (id tracker type) channel
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

(cl-defmethod helm-ypv-make-url ((bkm ypv-bookmark))
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
