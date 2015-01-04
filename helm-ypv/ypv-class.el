;;; class.el -*- lexical-binding: t -*-

;;;;; Bookmark Info

(require 'seq)

(cl-defmacro ypv-defclass (name slots)
  `(defclass ,name ()
     ,(seq-map
       (lambda (s)
         (if (listp s)
             (list (car s)
                   :initarg (intern (concat ":" (symbol-name (car s))))
                   :initform (cadr s)
                   :accessor (intern (concat (symbol-name name) "-" (symbol-name (car s)))))
           (list s
                 :initarg (intern (concat ":" (symbol-name s)))
                 :initform nil
                 :accessor (intern (concat (symbol-name name) "-"
                                           (symbol-name
                                            s))))))
       slots)))

(ypv-defclass ypv-bookmark
              ((yp "")
               (name "")
               (id "")
               (tracker "")
               (contact "")
               broadcasting))

(ypv-defclass ypv-channel
              ((yp "")
               (name "")
               (id "")
               (tracker "")
               (contact "")
               (genre "")
               (desc "")
               (bitrate "")
               (type "")
               (time "")
               (comment "")
               (listeners "")
               (relays "")
               broadcasting))


(provide 'ypv-class)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
