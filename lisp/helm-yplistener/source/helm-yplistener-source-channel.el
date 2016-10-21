;;; channel.el -*- lexical-binding: t -*-

;;;; Requires
(require 'cl-lib) ; don't use cl.el
(require 'seq)
(require 'helm)

(require 'colle)
(require 'glof)

;;;;; Local
(require 'helm-yplistener-global "helm-yplistener/helm-yplistener-global")
(require 'helm-yplistener-user-variable "helm-yplistener/helm-yplistener-user-variable")
(require 'helm-yplistener-face "helm-yplistener/helm-yplistener-face")
(require 'helm-yplistener-player "helm-yplistener/helm-yplistener-player")
(require 'helm-yplistener-url "helm-yplistener/helm-yplistener-url")

(require 'helm-yplistener-source-bookmark "helm-yplistener/source/helm-yplistener-source-bookmark")

;;;; Action
(cl-defun helm-yplistener-action-channel-open (channel)
  (cl-letf ((url (helm-yplistener-make-url `[:channel ,channel])))
    (thread-first channel
      helm-yplistener-bookmark-channel->bookmark
      (helm-yplistener-bookmark-data-update (helm-yplistener-bookmark-data-file)))
    (helm-yplistener-player helm-yplistener-player-type url)))

(cl-defun helm-yplistener-action-channel-copy-conctact-url (channel)
  (glof:let (((contact :contact))
             channel)
    (kill-new contact)
    (message "copy %s" contact)))

;;;; Canditate
(cl-defun helm-yplistener-channel-create-candidates (channels)
  (colle:map
   (lambda (info)
     (cons
      ;; display candidate
      (helm-yplistener-channel-create-display-candidate info)
      ;; real candidate
      info))
   channels))

(cl-defun helm-yplistener-channel-create-display-candidate (channel)
  (glof:let (((genre :genre)
              (desc :desc)
              (contact :contact)
              (bitrate :bitrate)
              (uptime :uptime)
              (comment :comment)
              (listeners :listeners)
              (relays :relays)
              (yp :yp)
              (type :type))
             channel)
    ;; (genre desc contact type bitrate uptime
    ;;        comment listeners relays yp)
    (cl-letf ((name (helm-yplistener-modify-channel-name channel))
              (genre (helm-yplistener-add-face genre 'helm-yplistener-genre))
              (desc (helm-yplistener-add-face desc 'helm-yplistener-desc))
              (contact (helm-yplistener-add-face contact 'helm-yplistener-contact))
              (type (helm-yplistener-add-face type 'helm-yplistener-type))
              (bitrate (helm-yplistener-add-face bitrate 'helm-yplistener-bitrate))
              (uptime (helm-yplistener-add-face uptime 'helm-yplistener-uptime))
              (comment (helm-yplistener-add-face comment 'helm-yplistener-comment))
              (lr (helm-yplistener-add-face
                   (concat listeners "/" relays) 'helm-yplistener-lr)))
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

(cl-defun helm-yplistener-modify-channel-name (channel)
  (helm-yplistener-add-face (glof:get channel :name)
                            (pcase channel
                              ((pred helm-yplistener-info-channel-p)
                               'font-lock-function-name-face)
                              ((pred helm-yplistener-channel-playable-p)
                               'helm-yplistener-name)
                              (_
                               'font-lock-variable-name-face))))

(cl-defun helm-yplistener-info-channel-p (channel)
  ;; return self.listeners()<-1;
  (glof:let (((listeners :listeners))
             channel)
    (and (stringp listeners)
       (cl-letf* ((num (string-to-number listeners)))
         (< num -1)))))

(cl-defun helm-yplistener-channel-playable-p (channel)
  ;; if (channel_id==null || channel_id==="" || channel_id===) return false;
  (glof:let (((id :id))
             channel)
    (not (or (string-equal
           id
           (make-string 32 ?0))
          (null id)
          (seq-empty-p id)))))

(defvar helm-yplistener-channel-candidate-channels nil)

(cl-defun helm-yplistener-channel-init ()
  (thread-last helm-yplistener-yp-urls
    helm-yplistener-get/parse-channels
    helm-yplistener-channel-create-candidates
    (setq helm-yplistener-channel-candidate-channels)))

;;;; Source

(defun helm-yplistener-channel-add-source-mark (name)
  (cl-letf ((mark "📺")) ; "\U0001F4FA"
    (cond ((window-system)
           (seq-concatenate 'string " " mark " "  name))
          (t
           name))))

(defclass helm-yplistener-channels-source (helm-source-sync)
  ((init :initform #'helm-yplistener-channel-init)
   (candidates :initform 'helm-yplistener-channel-candidate-channels)
   (action :initform
     (helm-make-actions
      "Open channel" #'helm-yplistener-action-channel-open
      "Add to bookmarks" #'helm-yplistener-action-bookmark-add
      "Copy contact url" #'helm-yplistener-action-channel-copy-conctact-url))
   (migemo :initform t)))

(defvar helm-source-yplistener-channels
  (helm-make-source (helm-yplistener-channel-add-source-mark "Channel list")
      'helm-yplistener-channels-source))

;;; Provide
(provide 'helm-yplistener-source-channel)
