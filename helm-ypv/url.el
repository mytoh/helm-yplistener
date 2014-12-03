;;; url -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(defmethod helm-ypv-make-url ((channel ypv-channel))
  (with-slots (id ip) channel
    (format "http://%s/pls/%s.pls?tip=%s"
            helm-ypv-local-address
            id ip)))

(defmethod helm-ypv-make-url ((bkm ypv-bookmark))
  (with-slots (id ip) bkm
    (format "http://%s/pls/%s.pls?tip=%s"
            helm-ypv-local-address
            id ip)))

(provide 'helm-ypv-url)

;;; url.el ends here
