;;; channel.el -*- lexical-binding: t -*-

;;;; Requires
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)
(require 'dash)
(require 's)
;;;;; Local
(require 'helm-ypv-global "helm-ypv/global")
(require 'helm-ypv-user-variable "helm-ypv/user-variable")
(require 'helm-ypv-face "helm-ypv/face")

(autoload 'helm-ypv-bookmark-action-add "helm-ypv/source/bookmark")

;;;; Channel
(cl-defstruct (ypv-channel
               (:constructor ypv-channel-new))
  (yp "")
  (name "")
  (id "")
  (ip "")
  (contact "")
  (genre "")
  (desc "")
  (bitrate "")
  (type "")
  (time "")
  (comment ""))

;;;; Action
(cl-defun helm-ypv-channel-action-open-channel (candidate)
  (cl-letf* ((info candidate)
             (url (helm-ypv-channel-make-url info)))
    (helm-ypv-player helm-ypv-player-type url)))

(cl-defun helm-ypv-channel-make-url (channel)
  (format "http://%s/pls/%s?tip=%s"
          helm-ypv-local-address
          (ypv-channel-id channel)
          (ypv-channel-ip channel)))

;;;; Canditate
(cl-defun helm-ypv-channel-create-candidates (channels)
  (-map
   #'(lambda (info)
       (cons
        ;; display candidate
        (helm-ypv-channel-create-display-candidate info)
        ;; real candidate
        info))
   channels))

(cl-defun helm-ypv-channel-create-display-candidate (channel)
  (cl-letf ((name (helm-ypv-add-face (ypv-channel-name channel) 'helm-ypv-name))
            (genre (helm-ypv-add-face (ypv-channel-genre channel) 'helm-ypv-genre))
            (desc (helm-ypv-add-face (ypv-channel-desc channel) 'helm-ypv-desc))
            (contact (helm-ypv-add-face (ypv-channel-contact channel) 'helm-ypv-contact))
            (type (helm-ypv-add-face (ypv-channel-type channel) 'helm-ypv-type))
            (bitrate (helm-ypv-add-face (ypv-channel-bitrate channel) 'helm-ypv-bitrate))
            (time (helm-ypv-add-face (ypv-channel-time channel) 'helm-ypv-time))
            (comment (helm-ypv-add-face (ypv-channel-comment channel) 'helm-ypv-comment)))
    (format "%-17.17s %s [%s] %s %s %s %s"
            name desc comment genre contact type time)))

(defvar helm-ypv-channel-candidate-channels nil)

(cl-defun helm-ypv-channel-init ()
  (setq helm-ypv-channel-candidate-channels
        (helm-ypv-channel-create-candidates (helm-ypv-get/parse-channels helm-ypv-yp-urls))))

;;;; Source

(defun helm-ypv-channel-add-source-mark (name)
  (cl-letf ((mark "📺"))
    (cond ((window-system)
           (cl-concatenate 'string " " mark " "  name))
          (t
           name))))

(defvar helm-source-ypv-channels
  `((name . ,(helm-ypv-channel-add-source-mark "Channel list"))
    (init . helm-ypv-channel-init)
    (candidates . helm-ypv-channel-candidate-channels)
    (action . (("Open channel" .  helm-ypv-channel-action-open-channel)
               ("Add to bookmarks" . helm-ypv-bookmark-action-add)))))

;;; Provide
(provide 'helm-ypv-source-channel)
