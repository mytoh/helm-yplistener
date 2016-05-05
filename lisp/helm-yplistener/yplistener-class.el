;;; class.el -*- lexical-binding: t -*-

;;;;; Bookmark Info

(require 'seq)

(require 'colle)

(cl-defmacro yplistener-defclass (name slots)
  `(defclass ,name ()
     ,(colle:map
       (lambda (s)
         (if (listp s)
             (list (car s)
                   :initarg (intern (concat ":" (helm-stringify (car s))))
                   :initform (cadr s)
                   :accessor (intern (concat (helm-stringify name) "-" (helm-stringify (car s)))))
           (list s
                 :initarg (intern (concat ":" (helm-stringify s)))
                 :initform nil
                 :accessor (intern (concat (helm-stringify name) "-"
                                           (helm-stringify
                                            s))))))
       slots)))

(yplistener-defclass yplistener-bookmark
                     ((yp "")
                      (name "")
                      (id "")
                      (tracker "")
                      (contact "")
                      (type "")
                      broadcasting))

(yplistener-defclass yplistener-channel
                     ((yp "")
                      (name "")
                      (id "")
                      (tracker "")
                      (contact "")
                      (genre "")
                      (desc "")
                      (bitrate "")
                      (type "")
                      (uptime "")
                      (comment "")
                      (listeners "")
                      (relays "")
                      broadcasting))


(provide 'yplistener-class)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
