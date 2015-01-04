;;; channel.el -*- lexical-binding: t -*-

;;;; Requires
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'seq)
(require 'helm)
;;;;; Local
(require 'ypv-class "helm-ypv/ypv-class")
(require 'helm-ypv-global "helm-ypv/helm-ypv-global")
(require 'helm-ypv-user-variable "helm-ypv/helm-ypv-user-variable")
(require 'helm-ypv-face "helm-ypv/helm-ypv-face")
(require 'helm-ypv-player "helm-ypv/helm-ypv-player")
(require 'helm-ypv-url "helm-ypv/helm-ypv-url")

(require 'helm-ypv-source-bookmark "helm-ypv/source/bookmark")

;;;; Action
(defmethod helm-ypv-action-channel-open ((channel ypv-channel))
  (cl-letf ((url (helm-ypv-make-url channel)))
    (cl-letf ((bookmark (helm-ypv-bookmark-channel->bookmark channel)))
      (helm-ypv-bookmark-data-update bookmark (helm-ypv-bookmark-data-file)))
    (helm-ypv-player helm-ypv-player-type url)))

(defmethod helm-ypv-action-channel-copy-conctact-url ((channel ypv-channel))
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

(defmethod helm-ypv-create-display-candidate ((channel ypv-channel))
  (cl-letf ((name (helm-ypv-modify-channel-name channel))
            (genre (helm-ypv-add-face (ypv-channel-genre channel) 'helm-ypv-genre))
            (desc (helm-ypv-add-face (ypv-channel-desc channel) 'helm-ypv-desc))
            (contact (helm-ypv-add-face (ypv-channel-contact channel) 'helm-ypv-contact))
            (type (helm-ypv-add-face (ypv-channel-type channel) 'helm-ypv-type))
            (bitrate (helm-ypv-add-face (ypv-channel-bitrate channel) 'helm-ypv-bitrate))
            (time (helm-ypv-add-face (ypv-channel-time channel) 'helm-ypv-time))
            (comment (helm-ypv-add-face (ypv-channel-comment channel) 'helm-ypv-comment))
            (listeners (helm-ypv-add-face (ypv-channel-listeners channel) 'helm-ypv-listeners)))
    (format "%-16.16s %-8.8s %-40.40s %+4s %+4s %7s %s %s"
            name
            genre
            desc
            bitrate
            time
            listeners
            type
            ;; yp
            ;; (if (string-empty-p comment) "" comment)
            contact
            )))

(defmethod helm-ypv-modify-channel-name ((channel ypv-channel))
  (helm-ypv-add-face (ypv-channel-name channel)
                     (cond ((helm-ypv-info-channel-p channel)
                            'font-lock-function-name-face)
                           ((helm-ypv-channel-playable-p channel)
                            'helm-ypv-name)
                           (t
                            'font-lock-variable-name-face))))

(defmethod helm-ypv-info-channel-p ((channel ypv-channel))
  ;; return self.listeners()<-1;
  (cl-letf* ((listeners (ypv-channel-listeners channel))
             (num (string-to-number (car (split-string listeners "/")))))
    (< num -1)))

(defmethod helm-ypv-channel-playable-p ((channel ypv-channel))
  ;; if (channel_id==null || channel_id==="" || channel_id===) return false;
  (cl-letf ((id (ypv-channel-id channel)))
    (not (or (string-equal
              id
              "00000000000000000000000000000000")
             (null id)
             (string-empty-p id)))))

(defvar helm-ypv-channel-candidate-channels nil)

(cl-defun helm-ypv-channel-init ()
  (setq helm-ypv-channel-candidate-channels
        (helm-ypv-channel-create-candidates (helm-ypv-get/parse-channels helm-ypv-yp-urls))))

;;;; Source

(defun helm-ypv-channel-add-source-mark (name)
  (cl-letf ((mark "📺")) ; "\U0001F4FA"
    (cond ((window-system)
           (seq-concatenate 'string " " mark " "  name))
          (t
           name))))

(defvar helm-source-ypv-channels
  `((name . ,(helm-ypv-channel-add-source-mark "Channel list"))
    (init . helm-ypv-channel-init)
    (candidates . helm-ypv-channel-candidate-channels)
    (action . (("Open channel" .  helm-ypv-action-channel-open)
               ("Add to bookmarks" . helm-ypv-action-bookmark-add)
               ("Copy contact url" . helm-ypv-action-channel-copy-conctact-url)))
    (migemo)))

;;; Provide
(provide 'helm-ypv-source-channel)
