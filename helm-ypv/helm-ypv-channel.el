
;;;; deps
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)
(require 'dash)
(require 's)

(require 'helm-ypv-global "helm-ypv/helm-ypv-global")
(require 'helm-ypv-bookmark "helm-ypv/helm-ypv-bookmark")


(cl-defstruct (ypv-channel
               (:constructor ypv-channel-new))
  (yp "")
  (name "")
  (id "")
  (ip "")
  (url "")
  (genre "")
  (desc "")
  (bitrate "")
  (type "")
  (time "")
  (comment ""))


(cl-defun helm-ypv-channel-action-open-channel (candidate)
  (cl-letf* ((info candidate)
             (url (helm-ypv-channel-make-url info)))
    (helm-ypv-player helm-ypv-player-type url)))

(cl-defun helm-ypv-channel-make-url (channel)
  (format "http://%s/pls/%s?tip=%s"
          helm-ypv-local-address
          (ypv-channel-id channel)
          (ypv-channel-ip channel)))

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
  (cl-letf ((format-string "%-17.17s %s [%s] %s %s %s %s")
            (name (helm-ypv-add-face (ypv-channel-name channel) 'helm-ypv-name-face))
            (genre (helm-ypv-add-face (ypv-channel-genre channel) 'helm-ypv-genre-face))
            (desc (helm-ypv-add-face (ypv-channel-desc channel) 'helm-ypv-desc-face))
            (url (helm-ypv-add-face (ypv-channel-url channel) 'helm-ypv-url-face))
            (type (helm-ypv-add-face (ypv-channel-type channel) 'helm-ypv-type-face))
            (bitrate (helm-ypv-add-face (ypv-channel-bitrate channel) 'helm-ypv-bitrate-face))
            (time (helm-ypv-add-face (ypv-channel-time channel) 'helm-ypv-time-face))
            (comment (helm-ypv-add-face (ypv-channel-comment channel) 'helm-ypv-comment-face)))
    (format format-string
            name
            desc
            comment
            genre
            url
            type
            time)))


(defvar helm-ypv-channel-candidate-channels nil)

(cl-defun helm-ypv-channel-init ()
  (setq helm-ypv-channel-candidate-channels
        (helm-ypv-channel-create-candidates (helm-ypv-get/parse-channels helm-ypv-yp-urls))))

(defvar helm-source-ypv-channels
  '((name . "Channel list")
    (init . helm-ypv-channel-init)
    (candidates . helm-ypv-channel-candidate-channels)
    (action . (("Open channel" .  helm-ypv-channel-action-open-channel)
               ("Add to bookmarks" . helm-ypv-bookmark-action-add)))))


(provide 'helm-ypv-channel)
