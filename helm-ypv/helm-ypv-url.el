;;; url -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(cl-defmethod helm-ypv-make-url ((channel ypv-channel))
  (with-slots (id tracker) channel
    (format "http://%s/pls/%s.pls?tip=%s"
            helm-ypv-local-address
            id tracker)))

(cl-defmethod helm-ypv-make-url ((bkm ypv-bookmark))
  (with-slots (id tracker) bkm
    (format "http://%s/pls/%s.pls?tip=%s"
            helm-ypv-local-address
            id tracker)))

(provide 'helm-ypv-url)

;;; url.el ends here
