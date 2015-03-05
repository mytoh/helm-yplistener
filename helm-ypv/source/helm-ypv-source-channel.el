;;; channel.el -*- lexical-binding: t -*-

;;;; Requires
(require 'cl-lib) ; don't use cl.el
(require 'seq)
(require 'helm)
;;;;; Local
(require 'ypv-class "helm-ypv/ypv-class")
(require 'helm-ypv-global "helm-ypv/helm-ypv-global")
(require 'helm-ypv-user-variable "helm-ypv/helm-ypv-user-variable")
(require 'helm-ypv-face "helm-ypv/helm-ypv-face")
(require 'helm-ypv-player "helm-ypv/helm-ypv-player")
(require 'helm-ypv-url "helm-ypv/helm-ypv-url")

(require 'helm-ypv-source-bookmark "helm-ypv/source/helm-ypv-source-bookmark")

;;;; Action
(cl-defmethod helm-ypv-action-channel-open ((channel ypv-channel))
  (cl-letf ((url (helm-ypv-make-url channel)))
    (cl-letf ((bookmark (helm-ypv-bookmark-channel->bookmark channel)))
      (helm-ypv-bookmark-data-update bookmark (helm-ypv-bookmark-data-file)))
    (helm-ypv-player helm-ypv-player-type url)))

(cl-defmethod helm-ypv-action-channel-copy-conctact-url ((channel ypv-channel))
  (with-slots (contact) channel
    (kill-new contact)
    (message "copy %s" contact)))

;;;; Canditate
(cl-defun helm-ypv-channel-create-candidates (channels)
  (seq-map
   (lambda (info)
     (cons
      ;; display candidate
      (helm-ypv-create-display-candidate info)
      ;; real candidate
      info))
   channels))

(cl-defmethod helm-ypv-create-display-candidate ((channel ypv-channel))
  (with-slots (genre desc contact type bitrate uptime
                     comment listeners relays yp)
      channel
    (cl-letf ((name (helm-ypv-modify-channel-name channel))
              (genre (helm-ypv-add-face genre 'helm-ypv-genre))
              (desc (helm-ypv-add-face desc 'helm-ypv-desc))
              (contact (helm-ypv-add-face contact 'helm-ypv-contact))
              (type (helm-ypv-add-face type 'helm-ypv-type))
              (bitrate (helm-ypv-add-face bitrate 'helm-ypv-bitrate))
              (uptime (helm-ypv-add-face uptime 'helm-ypv-uptime))
              (comment (helm-ypv-add-face comment 'helm-ypv-comment))
              (lr (helm-ypv-add-face
                   (concat listeners "/" relays) 'helm-ypv-lr)))
      (format "%-16.16s %-8.8s %-40.40s %-40.40s %+4s %+4s %7s %s %s %s"
              name
              genre
              desc
              comment
              bitrate
              uptime
              lr
              type
              yp
              ;; (if (string-empty-p comment) "" comment)
              contact
              ))))

(cl-defmethod helm-ypv-modify-channel-name ((channel ypv-channel))
  (helm-ypv-add-face (ypv-channel-name channel)
                     (cond ((helm-ypv-info-channel-p channel)
                            'font-lock-function-name-face)
                           ((helm-ypv-channel-playable-p channel)
                            'helm-ypv-name)
                           (t
                            'font-lock-variable-name-face))))

(cl-defmethod helm-ypv-info-channel-p ((channel ypv-channel))
  ;; return self.listeners()<-1;
  (with-slots (listeners) channel
    (cl-letf* ((num (string-to-number listeners)))
      (< num -1))))

(cl-defmethod helm-ypv-channel-playable-p ((channel ypv-channel))
  ;; if (channel_id==null || channel_id==="" || channel_id===) return false;
  (with-slots (id) channel
    (not (or (string-equal
              id
              "00000000000000000000000000000000")
             (null id)
             (string-empty-p id)))))

(defvar helm-ypv-channel-candidate-channels nil)

(cl-defun helm-ypv-channel-init ()
  (thread-last helm-ypv-yp-urls
    helm-ypv-get/parse-channels
    helm-ypv-channel-create-candidates
    (setq helm-ypv-channel-candidate-channels)))

;;;; Source

(defun helm-ypv-channel-add-source-mark (name)
  (cl-letf ((mark "📺")) ; "\U0001F4FA"
    (cond ((window-system)
           (seq-concatenate 'string " " mark " "  name))
          (t
           name))))

(defclass helm-ypv-channels-source (helm-source-sync)
  ((init :initform #'helm-ypv-channel-init)
   (candidates :initform 'helm-ypv-channel-candidate-channels)
   (action :initform
           (helm-make-actions
            "Open channel" #'helm-ypv-action-channel-open
            "Add to bookmarks" #'helm-ypv-action-bookmark-add
            "Copy contact url" #'helm-ypv-action-channel-copy-conctact-url))))

(defvar helm-source-ypv-channels
  (helm-make-source (helm-ypv-channel-add-source-mark "Channel list")
      'helm-ypv-channels-source))

;;; Provide
(provide 'helm-ypv-source-channel)
