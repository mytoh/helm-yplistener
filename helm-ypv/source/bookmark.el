;;; helm-ypv-bookmark

;;;;; deps
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)
(require 'dash)
(require 's)

(require 'helm-ypv-global "helm-ypv/global")
(require 'helm-ypv-user-variable "helm-ypv/user-variable")
(require 'helm-ypv-face "helm-ypv/face")

;;;;; internal

(cl-defun helm-ypv-bookmark-make-url (bkm)
  (format "http://%s/pls/%s?tip=%s"
          helm-ypv-local-address
          (ypv-bookmark-id bkm)
          (ypv-bookmark-ip bkm)))

(cl-defun helm-ypv-bookmark-compare-id (bmk1 bmk2)
  (equal (ypv-bookmark-id bmk1)
         (ypv-bookmark-id bmk2)))

;;;;; bookmark data file

(cl-defun helm-ypv-bookmark-data-write (file data)
  (with-temp-file file
    (cl-letf ((standard-output (current-buffer)))
      (prin1 data))))

(cl-defun helm-ypv-bookmark-data-add (file data)
  (if (file-exists-p file)
      (if (helm-ypv-bookmark-data-channel-exists-p file data)
          (helm-ypv-bookmark-data-update file data)
        (helm-ypv-bookmark-data-append file data))
    (helm-ypv-bookmark-data-write file (list data))))

(cl-defun helm-ypv-bookmark-data-append (file data)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "updating bookmark")
    (helm-ypv-bookmark-data-write file (cl-concatenate 'list old (list data)))
    (message (format "added %s" data))))

(cl-defun helm-ypv-bookmark-data-update (file data)
  (cl-letf ((old (cl-remove-if
                  (lambda (old-bookmark)
                    (helm-ypv-bookmark-compare-id
                     old-bookmark data))
                  (helm-ypv-bookmark-data-read file))))
    (message "updating bookmark")
    (helm-ypv-bookmark-data-write file (cl-concatenate 'list old (list data)))
    (message (format "update to add %s" data))))

(cl-defun helm-ypv-bookmark-data-remove (file bookmark)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "removing bookmark")
    (helm-ypv-bookmark-data-write file
                                  (cl-remove-if
                                   (lambda (old-bookmark)
                                     (helm-ypv-bookmark-compare-id
                                      old-bookmark bookmark))
                                   old))
    (message (format "removed %s" bookmark))))

(cl-defun helm-ypv-bookmark-data-channel-exists-p (file bkm)
  (cl-find bkm (helm-ypv-bookmark-data-read file)
           :test #'equal))

(cl-defun helm-ypv-bookmark-data-read (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(cl-defun helm-ypv-bookmark-data-file ()
  (expand-file-name "helm-ypv-bookmarks" user-emacs-directory))

;;;;; bookmark info

(cl-defstruct (ypv-bookmark
               (:constructor ypv-bookmark-new))
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
  (comment "")
  (broadcasting nil))

(cl-defun helm-ypv-bookmark-channel->bookmark (channel)
  (ypv-bookmark-new
   :yp (ypv-channel-yp channel)
   :name (ypv-channel-name channel)
   :id (ypv-channel-id channel)
   :ip (ypv-channel-ip channel)
   :url (ypv-channel-url channel)
   :genre (ypv-channel-genre channel)
   :desc (ypv-channel-desc channel)
   :bitrate (ypv-channel-bitrate channel)
   :type (ypv-channel-type channel)
   :time (ypv-channel-time  channel)
   :comment (ypv-channel-comment channel)
   :broadcasting nil))


;;;;; action

(cl-defun helm-ypv-bookmark-action-add (candidate)
  (cl-letf* ((info (helm-ypv-bookmark-channel->bookmark candidate)))
    (helm-ypv-bookmark-data-add (helm-ypv-bookmark-data-file) info)))

(cl-defun helm-ypv-bookmark-action-remove (candidate)
  (cl-letf* ((info candidate))
    (helm-ypv-bookmark-data-remove (helm-ypv-bookmark-data-file) info)))

(cl-defun helm-ypv-bookmark-action-open-channel (candidate)
  (cl-letf* ((bookmark candidate)
             (url (helm-ypv-bookmark-make-url bookmark)))
    (helm-ypv-bookmark-data-update (helm-ypv-bookmark-data-file) bookmark)
    (helm-ypv-player helm-ypv-player-type url)))

;;;;; candidate

(cl-defun helm-ypv-bookmark-create-display-candidate (bookmark)
  (cl-letf ((format-string "%-17.17s %s")
            (name (helm-ypv-add-face (ypv-bookmark-name bookmark) (if (ypv-bookmark-broadcasting bookmark)
                                                                      'helm-ypv-name-face
                                                                    'font-lock-dcc-face)))
            (id (helm-ypv-add-face (ypv-bookmark-id bookmark) 'helm-ypv-id-face)))
    (format format-string
            name
            id)))

(cl-defun helm-ypv-bookmark-channel-broadcasting-p (bookmark channels)
  (cl-find-if #'(lambda (channel)
                  (equal (ypv-bookmark-id bookmark)
                         (ypv-channel-id channel)))
              channels))

(cl-defun helm-ypv-bookmark-find-broadcasting-channels (bookmarks channels)
  (cl-remove-if
   #'(lambda (b) (eq b nil))
   (cl-map 'list
           #'(lambda (bookmark)
               (if (helm-ypv-bookmark-channel-broadcasting-p bookmark channels)
                   (helm-ypv-bookmark-set-broadcasting bookmark t)
                 (helm-ypv-bookmark-set-broadcasting bookmark nil)))
           bookmarks)))

(cl-defun helm-ypv-bookmark-set-broadcasting (obj value)
  (setf (ypv-bookmark-broadcasting obj) value)
  obj)

(cl-defun helm-ypv-bookmark-create-candidates (channels)
  (if (not (file-exists-p (helm-ypv-bookmark-data-file)))
      '()
    (-map
     #'(lambda (bookmark)
         (cons
          ;; display candidate
          (helm-ypv-bookmark-create-display-candidate bookmark)
          ;; real candidate
          bookmark))
     (helm-ypv-bookmark-find-broadcasting-channels
      (helm-ypv-bookmark-data-read (helm-ypv-bookmark-data-file))
      channels))))

;;;;; source


(defvar helm-ypv-candidate-bookmarks nil)

(cl-defun helm-ypv-bookmark-init ()
  (setq helm-ypv-candidate-bookmarks
        (helm-ypv-bookmark-create-candidates (helm-ypv-get/parse-channels helm-ypv-yp-urls))))

(defvar helm-source-ypv-bookmarks
  '((name . "Bookmarks")
    (init . helm-ypv-bookmark-init)
    (candidates . helm-ypv-candidate-bookmarks)
    (action . (("Open channel" . helm-ypv-bookmark-action-open-channel)
               ("Remove bookmark" . helm-ypv-bookmark-action-remove)))))



(provide 'helm-ypv-bookmark)
