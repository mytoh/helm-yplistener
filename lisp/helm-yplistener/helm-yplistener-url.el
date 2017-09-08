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
    (pcase (downcase type)
      ("web" (string-trim (glof:get channel :contact)))
      (_
       (format "%s://%s:%s/stream/%s.%s?tip=%s"
               (pcase (downcase type)
                 ((rx (or "flv" "mkv"))
                  "http")
                 (_
                  helm-yplistener-default-protocol))
               (glof:get helm-yplistener-local-address
                       :host)
               (glof:get helm-yplistener-local-address
                       :port)
               id type tracker)))))

(cl-defun helm-yplistener-make-url-bookmark (bkm)
  (glof:let (((id :id)
            (tracker :tracker)
            (type :type))
           bkm)
    (format "%s://%s:%s/stream/%s.%s?tip=%s"
            (pcase (downcase type)
              ((rx (or "flv" "mkv"))
               
               "http")
              (_ helm-yplistener-default-protocol))
            (glof:get helm-yplistener-local-address
                    :host)
            (glof:get helm-yplistener-local-address
                    :port)
            id type tracker)))

(provide 'helm-yplistener-url)

;;; url.el ends here
