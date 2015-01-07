;;; bookmark.el -*- lexical-binding: t -*-

;;;; Requires
(eval-when-compile (require 'cl-lib)) ; don't use cl.el (require 'helm)
(require 'seq)
;;;;; Local
(require 'ypv-class "helm-ypv/ypv-class")
(require 'helm-ypv-user-variable "helm-ypv/helm-ypv-user-variable")
(require 'helm-ypv-player "helm-ypv/helm-ypv-player")
(require 'helm-ypv-face "helm-ypv/helm-ypv-face")
(require 'helm-ypv-url "helm-ypv/helm-ypv-url")

(autoload 'helm-ypv-get/parse-channels "helm-ypv/helm-ypv-global")
(autoload 'helm-ypv-player "helm-ypv/helm-ypv-global")


;;;; Functions
;;;;; Utils

(cl-defun helm-ypv-bookmark-equal-id (bmk1 bmk2)
  (equal (ypv-bookmark-id bmk1)
         (ypv-bookmark-id bmk2)))

(cl-defun helm-ypv-bookmark-equal-name (bmk1 bmk2)
  (equal (ypv-bookmark-name bmk1)
         (ypv-bookmark-name bmk2)))

(defmethod helm-ypv-bookmark-remove-if-name ((new-bookmark ypv-bookmark) bookmarks)
  (seq-remove (lambda (old-bookmark)
                (helm-ypv-bookmark-equal-name
                 old-bookmark new-bookmark))
              bookmarks))

;;;;; Bookmark Data
(cl-defun helm-ypv-bookmark-data-write (file data)
  (with-temp-file file (cl-letf ((standard-output (current-buffer)))
                         (prin1 data))))

(defmethod helm-ypv-bookmark-data-add ((bookmark ypv-bookmark) file)
  (if (file-exists-p file)
      (if (helm-ypv-bookmark-data-channel-exists-p bookmark file)
          (helm-ypv-bookmark-data-update bookmark file)
        (helm-ypv-bookmark-data-append bookmark file))
    (helm-ypv-bookmark-data-write file (list bookmark))))

(defmethod helm-ypv-bookmark-data-append ((bookmark ypv-bookmark) file)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "updating bookmark")
    (thread-last bookmark
      list (seq-concatenate 'list old)
      (helm-ypv-bookmark-data-write file))
    (message (format "added %s" bookmark))))

(defmethod helm-ypv-bookmark-data-update ((bookmark ypv-bookmark) file)
  (cl-letf ((new (thread-last file
                   helm-ypv-bookmark-data-read (helm-ypv-bookmark-remove-if-name
                                                bookmark))))
    (if (helm-ypv-bookmark-data-channel-exists-p bookmark file)
        (cl-locally (message "updating bookmark")
          (helm-ypv-bookmark-data-write file (seq-concatenate 'list new (list bookmark)))
          (message (format "update to add %s" bookmark)))
      (message "channel not found on bookmark"))))

(defmethod helm-ypv-bookmark-data-remove ((bookmark ypv-bookmark) file)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "removing bookmark")
    (helm-ypv-bookmark-data-write file (helm-ypv-bookmark-remove-if-name
                                        bookmark old))
    (message (format "removed %s" bookmark))))

(defmethod helm-ypv-bookmark-data-channel-exists-p ((bkm ypv-bookmark) file)
  (cl-find bkm (helm-ypv-bookmark-data-read file)
           :test #'helm-ypv-bookmark-equal-name))

(cl-defun helm-ypv-bookmark-data-read (file)
  (with-temp-buffer (insert-file-contents file)
                    (read (current-buffer))))

(cl-defun helm-ypv-bookmark-data-file ()
  (expand-file-name "helm-ypv-bookmarks" user-emacs-directory))


(defmethod helm-ypv-bookmark-channel->bookmark ((channel ypv-channel))
  (with-slots (yp name id tracker contact) channel
    (make-instance 'ypv-bookmark
                   name
                   :yp yp
                   :name name
                   :id id
                   :tracker tracker
                   :contact contact
                   :broadcasting nil)))

;;;;; Action
(defmethod helm-ypv-action-bookmark-add ((channel ypv-channel))
  (cl-letf ((bookmark (helm-ypv-bookmark-channel->bookmark channel)))
    (helm-ypv-bookmark-data-add bookmark (helm-ypv-bookmark-data-file))))

(defmethod helm-ypv-action-bookmark-remove ((bookmark ypv-bookmark))
  (helm-ypv-bookmark-data-remove bookmark (helm-ypv-bookmark-data-file)))

(defmethod helm-ypv-action-bookmark-open ((bookmark ypv-bookmark))
  (cl-letf ((url (helm-ypv-make-url bookmark)))
    (helm-ypv-bookmark-data-update bookmark (helm-ypv-bookmark-data-file))
    (helm-ypv-player helm-ypv-player-type url)))

;;;;; Candidate
(defmethod helm-ypv-create-display-candidate ((bookmark ypv-bookmark))
  (cl-letf ((format-string "%-17.17s %s")
            (name (helm-ypv-add-face (ypv-bookmark-name bookmark)
                                     (if (ypv-bookmark-broadcasting bookmark)
                                         'helm-ypv-name
                                       'font-lock-comment-face)))
            (id (helm-ypv-add-face (ypv-bookmark-id bookmark) 'helm-ypv-id)))
    (format format-string
            name
            id)))

(defmethod helm-ypv-bookmark-channel-broadcasting-p ((bookmark ypv-bookmark) channels)
  (cl-find-if (lambda (channel)
                (equal (ypv-bookmark-id bookmark)
                       (ypv-channel-id channel)))
              channels))

(cl-defun helm-ypv-bookmark-find-broadcasting-channels (bookmarks channels)
  (seq-remove (lambda (b) (eq b nil))
              (seq-map (lambda (bookmark)
                         (if (helm-ypv-bookmark-channel-broadcasting-p bookmark channels)
                             (helm-ypv-bookmark-set-broadcasting bookmark t)
                           (helm-ypv-bookmark-set-broadcasting bookmark nil)))
                       bookmarks)))

(defmethod helm-ypv-bookmark-set-broadcasting ((obj ypv-bookmark) value)
  (setf (ypv-bookmark-broadcasting obj) value)
  obj)

(cl-defun helm-ypv-bookmark-create-candidates (channels)
  (if (not (file-exists-p (helm-ypv-bookmark-data-file)))
      '()
    (seq-map (lambda (bookmark)
               (cons
                ;; display candidate
                (helm-ypv-create-display-candidate bookmark)
                ;; real candidate
                bookmark))
             (helm-ypv-bookmark-find-broadcasting-channels
              (helm-ypv-bookmark-data-read (helm-ypv-bookmark-data-file))
              channels))))

;;;;; Source
(defvar helm-ypv-candidate-bookmarks nil)

(cl-defun helm-ypv-bookmark-init ()
  (setq helm-ypv-candidate-bookmarks
        (helm-ypv-bookmark-create-candidates
         (helm-ypv-get/parse-channels helm-ypv-yp-urls))))

(defun helm-ypv-bookmark-add-source-mark (name)
  (cl-letf ((mark "🔖")) ; "\U0001F516"
    (cond ((window-system)
           (seq-concatenate 'string " " mark " "  name))
          (t
           name))))

(defvar helm-source-ypv-bookmarks
  `((name . ,(helm-ypv-bookmark-add-source-mark "Bookmarks"))
    (init . helm-ypv-bookmark-init)
    (candidates . helm-ypv-candidate-bookmarks)
    (action . (("Open channel" . helm-ypv-action-bookmark-open)
               ("Remove bookmark" . helm-ypv-action-bookmark-remove)))
    (migemo)))


;;;; Provide
(provide 'helm-ypv-source-bookmark)
