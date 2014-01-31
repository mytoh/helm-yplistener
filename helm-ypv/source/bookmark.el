;;; bookmark.el -*- lexical-binding: t -*-

;;;; Requires
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)
(require 'dash)
(require 's)
;;;;; Local
(require 'helm-ypv-user-variable "helm-ypv/user-variable")
(require 'helm-ypv-player "helm-ypv/player")
(require 'helm-ypv-face "helm-ypv/face")

(autoload 'helm-ypv-get/parse-channels "helm-ypv/global")
(autoload 'helm-ypv-player "helm-ypv/global")

;;;; Functions
;;;;; Utils
(cl-defun helm-ypv-bookmark-make-url (bkm)
  (format "http://%s/pls/%s?tip=%s"
          helm-ypv-local-address
          (ypv-bookmark-id bkm)
          (ypv-bookmark-ip bkm)))

(cl-defun helm-ypv-bookmark-equal-id (bmk1 bmk2)
  (equal (ypv-bookmark-id bmk1)
         (ypv-bookmark-id bmk2)))

(cl-defun helm-ypv-bookmark-equal-name (bmk1 bmk2)
  (equal (ypv-bookmark-name bmk1)
         (ypv-bookmark-name bmk2)))

;;;;; Bookmark Data

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
  (cl-letf* ((old (helm-ypv-bookmark-data-read file))
             (new (cl-remove-if
                   (lambda (old-bookmark)
                     (helm-ypv-bookmark-equal-name
                      old-bookmark data))
                   old)))
    (message "updating bookmark")
    (helm-ypv-bookmark-data-write file (cl-concatenate 'list new (list data)))
    (message (format "update to add %s" data))))

(cl-defun helm-ypv-bookmark-data-remove (file bookmark)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "removing bookmark")
    (helm-ypv-bookmark-data-write file
                                  (cl-remove-if
                                   (lambda (old-bookmark)
                                     (helm-ypv-bookmark-equal-name
                                      old-bookmark bookmark))
                                   old))
    (message (format "removed %s" bookmark))))

(cl-defun helm-ypv-bookmark-data-channel-exists-p (file bkm)
  (cl-find bkm (helm-ypv-bookmark-data-read file)
           :test 'helm-ypv-bookmark-equal-name))

(cl-defun helm-ypv-bookmark-data-read (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(cl-defun helm-ypv-bookmark-data-file ()
  (expand-file-name "helm-ypv-bookmarks" user-emacs-directory))

;;;;; Bookmark Info

(cl-defstruct (ypv-bookmark
               (:constructor ypv-bookmark-new))
  (yp "")
  (name "")
  (id "")
  (ip "")
  (contact "")
  (broadcasting nil))

(cl-defun helm-ypv-bookmark-channel->bookmark (channel)
  (ypv-bookmark-new
   :yp (ypv-channel-yp channel)
   :name (ypv-channel-name channel)
   :id (ypv-channel-id channel)
   :ip (ypv-channel-ip channel)
   :contact (ypv-channel-contact channel)
   :broadcasting nil))


;;;;; Action

(cl-defun helm-ypv-bookmark-action-add (_candidate)
  (cl-letf ((bookmark (helm-ypv-bookmark-channel->bookmark _candidate)))
    (helm-ypv-bookmark-data-add (helm-ypv-bookmark-data-file) bookmark)))

(cl-defun helm-ypv-bookmark-action-remove (_candidate)
  (cl-letf ((bookmark _candidate))
    (helm-ypv-bookmark-data-remove (helm-ypv-bookmark-data-file) bookmark)))



(cl-defun helm-ypv-bookmark-action-open-channel (candidate)
  (cl-letf* ((bookmark candidate)
             (url (helm-ypv-bookmark-make-url bookmark)))
    (helm-ypv-bookmark-data-update (helm-ypv-bookmark-data-file) bookmark)
    (helm-ypv-player helm-ypv-player-type url)))

;;;;; Candidate

(cl-defun helm-ypv-bookmark-create-display-candidate (bookmark)
  (cl-letf ((format-string "%-17.17s %s")
            (name (helm-ypv-add-face (ypv-bookmark-name bookmark) (if (ypv-bookmark-broadcasting bookmark)
                                                                      'helm-ypv-name
                                                                    'font-lock-comment-face)))
            (id (helm-ypv-add-face (ypv-bookmark-id bookmark) 'helm-ypv-id)))
    (format format-string
            name
            id)))

(cl-defun helm-ypv-bookmark-channel-broadcasting-p (bookmark channels)
  (cl-find-if (lambda (channel)
                (equal (ypv-bookmark-id bookmark)
                       (ypv-channel-id channel)))
              channels))

(cl-defun helm-ypv-bookmark-find-broadcasting-channels (bookmarks channels)
  (cl-remove-if
   (lambda (b) (eq b nil))
   (cl-map 'list
           (lambda (bookmark)
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
     (lambda (bookmark)
       (cons
        ;; display candidate
        (helm-ypv-bookmark-create-display-candidate bookmark)
        ;; real candidate
        bookmark))
     (helm-ypv-bookmark-find-broadcasting-channels
      (helm-ypv-bookmark-data-read (helm-ypv-bookmark-data-file))
      channels))))

;;;;; Source

(defvar helm-ypv-candidate-bookmarks nil)

(cl-defun helm-ypv-bookmark-init ()
  (setq helm-ypv-candidate-bookmarks
        (helm-ypv-bookmark-create-candidates (helm-ypv-get/parse-channels helm-ypv-yp-urls))))

(defun helm-ypv-bookmark-add-source-mark (name)
  (cl-letf ((mark "🔖" )) ; "\U0001F516"
    (cond ((window-system)
           (cl-concatenate 'string " " mark " "  name))
          (t
           name))))

(defvar helm-source-ypv-bookmarks
  `((name . ,(helm-ypv-bookmark-add-source-mark "Bookmarks"))
    (init . helm-ypv-bookmark-init)
    (candidates . helm-ypv-candidate-bookmarks)
    (action . (("Open channel" . helm-ypv-bookmark-action-open-channel)
               ("Remove bookmark" . helm-ypv-bookmark-action-remove)))))


;;;; Provide
(provide 'helm-ypv-source-bookmark)
