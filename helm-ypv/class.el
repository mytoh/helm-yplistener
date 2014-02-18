;;; class.el -*- lexical-binding: t -*-

;;;;; Bookmark Info

(defclass ypv-bookmark ()
  ((yp :initarg :yp
       :type string
       :initform ""
       :accessor ypv-bookmark-yp)
   (name :initarg :name
         :type string
         :initform ""
         :accessor ypv-bookmark-name)
   (id :initarg :id
       :type string
       :initform ""
       :accessor ypv-bookmark-id)
   (ip :initarg :ip
       :type string
       :initform ""
       :accessor ypv-bookmark-ip)
   (contact :initarg :contact
            :type string
            :initform ""
            :accessor ypv-bookmark-contact)
   (broadcasting :initarg :broadcasting
                 :initform nil
                 :type symbol
                 :accessor ypv-bookmark-broadcasting)))

(defclass ypv-channel ()
  ((yp :initarg :yp
       :type string
       :initform ""
       :accessor ypv-channel-yp)
   (name :initarg :name
         :type string
         :initform ""
         :accessor ypv-channel-name)
   (id :initarg :id
       :type string
       :initform ""
       :accessor ypv-channel-id)
   (ip :initarg :ip
       :type string
       :initform ""
       :accessor ypv-channel-ip)
   (contact :initarg :contact
            :type string
            :initform ""
            :accessor ypv-channel-contact)
   (genre :initarg :genre
          :type string
          :initform ""
          :accessor ypv-channel-genre
          )
   (desc :initarg :desc
         :type string
         :initform ""
         :accessor ypv-channel-desc)
   (bitrate :initarg :bitrate
            :initform  ""
            :accessor ypv-channel-bitrate)
   (type :initarg :type
         :initform ""
         :type string
         :accessor ypv-channel-type)
   (time :initarg :time
         :initform ""
         :type string
         :accessor ypv-channel-time)
   (comment :initarg :comment
            :initform ""
            :type string
            :accessor ypv-channel-comment)))

(provide 'helm-ypv-class)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
