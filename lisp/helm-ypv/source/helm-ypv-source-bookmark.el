;;; bookmark.el -*- lexical-binding: t -*-

;;;; Requires
(require 'cl-lib) ; don't use cl.el (require 'helm)
(require 'seq)

(require 'colle)
(require 'glof)

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
  (equal (eieio-oref bmk1 'id)
         (eieio-oref bmk2 'id)))

(cl-defun helm-ypv-bookmark-equal-name (bmk1 bmk2)
  (equal (eieio-oref bmk1 'name)
         (eieio-oref bmk2 'name)))

(cl-defun helm-ypv-bookmark-remove-if-name (new-bookmark bookmarks)
  (seq-remove (lambda (old-bookmark)
                (helm-ypv-bookmark-equal-name
                 old-bookmark new-bookmark))
              bookmarks))

;;;;; Bookmark Data
(cl-defun helm-ypv-bookmark-data-write (file data)
  (with-temp-file file (cl-letf ((standard-output (current-buffer)))
                         (prin1 data))))

(cl-defun helm-ypv-bookmark-data-add (bookmark file)
  (if (file-exists-p file)
      (if (helm-ypv-bookmark-data-channel-exists-p bookmark file)
          (helm-ypv-bookmark-data-update bookmark file)
        (helm-ypv-bookmark-data-append bookmark file))
    (helm-ypv-bookmark-data-write file (list bookmark))))

(cl-defun helm-ypv-bookmark-data-append (bookmark file)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "updating bookmark")
    (thread-last bookmark
      list
      (seq-concatenate 'list old)
      (helm-ypv-bookmark-data-write file))
    (message (format "added %s" bookmark))))

(cl-defun helm-ypv-bookmark-data-update (bookmark file)
  (cl-letf ((new (thread-last file
                   helm-ypv-bookmark-data-read
                   (helm-ypv-bookmark-remove-if-name bookmark))))
    (if (helm-ypv-bookmark-data-channel-exists-p bookmark file)
        (cl-locally (message "updating bookmark")
          (helm-ypv-bookmark-data-write file (seq-concatenate 'list new (list bookmark)))
          (message (format "update to add %s" bookmark)))
      (message "channel not found on bookmark"))))

(cl-defun helm-ypv-bookmark-data-remove (bookmark file)
  (cl-letf ((old (helm-ypv-bookmark-data-read file)))
    (message "removing bookmark")
    (helm-ypv-bookmark-data-write file
                                  (helm-ypv-bookmark-remove-if-name
                                   bookmark old))
    (message (format "removed %s" bookmark))))

(cl-defun helm-ypv-bookmark-data-channel-exists-p (bkm file)
  (cl-find bkm (helm-ypv-bookmark-data-read file)
           :test #'helm-ypv-bookmark-equal-name))

(cl-defun helm-ypv-bookmark-data-read (file)
  (with-temp-buffer (insert-file-contents file)
                    (read (current-buffer))))

(cl-defun helm-ypv-bookmark-data-file ()
  (expand-file-name
   helm-ypv-bookmark-file-name
   (expand-file-name
    helm-ypv-bookmark-directory-name
    user-emacs-directory)))


(cl-defun helm-ypv-bookmark-channel->bookmark (channel)
  (glof:let (((yp :yp)
              (name :name)
              (id :id)
              (tracker :tracker)
              (contact :contact)
              (type :type))
             channel)
    (make-instance 'ypv-bookmark
                   :yp yp
                   :name name
                   :id id
                   :tracker tracker
                   :contact contact
                   :type type
                   :broadcasting nil)))

(cl-defun helm-ypv-ensure-bookmark-directory (path)
  (unless (file-exists-p path)
    (make-directory path)))


;;;;; Action
(cl-defun helm-ypv-action-bookmark-add (channel)
  (helm-ypv-ensure-bookmark-directory
   (file-name-directory (helm-ypv-bookmark-data-file)))
  (thread-first channel
    helm-ypv-bookmark-channel->bookmark
    (helm-ypv-bookmark-data-add (helm-ypv-bookmark-data-file))))

(cl-defun helm-ypv-action-bookmark-remove (bookmark)
  (helm-ypv-bookmark-data-remove bookmark (helm-ypv-bookmark-data-file)))

(cl-defun helm-ypv-action-bookmark-open (bookmark)
  (cl-letf ((url (helm-ypv-make-url `[:bookmark ,bookmark])))
    (helm-ypv-bookmark-data-update bookmark (helm-ypv-bookmark-data-file))
    (helm-ypv-player helm-ypv-player-type url)))

;;;;; Candidate
(cl-defun helm-ypv-bookmark-create-display-candidate (bookmark)
  (cl-letf ((format-string "%-17.17s %s")
            (name (helm-ypv-add-face (eieio-oref bookmark 'name)
                                     (if (eieio-oref bookmark 'broadcasting)
                                         'helm-ypv-name
                                       'font-lock-comment-face)))
            (id (helm-ypv-add-face (eieio-oref bookmark 'id) 'helm-ypv-id)))
    (format format-string
            name
            id)))

(cl-defun helm-ypv-bookmark-channel-broadcasting-p (bookmark channels)
  (cl-find-if (lambda (channel)
                (equal (eieio-oref bookmark 'id)
                       (glof:get channel :id)))
              channels))

(cl-defun helm-ypv-bookmark-find-broadcasting-channels (bookmarks channels)
  (seq-remove #'null
              (colle:map (lambda (bookmark)
                        (if (helm-ypv-bookmark-channel-broadcasting-p bookmark channels)
                            (helm-ypv-bookmark-set-broadcasting bookmark t)
                          (helm-ypv-bookmark-set-broadcasting bookmark nil)))
                      bookmarks)))

(cl-defun helm-ypv-bookmark-set-broadcasting (obj value)
  (setf (eieio-oref obj 'broadcasting) value)
  obj)

(cl-defun helm-ypv-bookmark-create-candidates (channels)
  (if (not (file-exists-p (helm-ypv-bookmark-data-file)))
      '()
    (seq-map (lambda (bookmark)
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
        (helm-ypv-bookmark-create-candidates
         (helm-ypv-get/parse-channels helm-ypv-yp-urls))))

(defun helm-ypv-bookmark-add-source-mark (name)
  (cl-letf ((mark "🔖")) ; "\U0001F516"
    (cond ((window-system)
           (seq-concatenate 'string " " mark " "  name))
          (t
           name))))

(defclass helm-ypv-bookmarks-source (helm-source-sync)
  ((init  :initform #'helm-ypv-bookmark-init)
   (candidates :initform 'helm-ypv-candidate-bookmarks)
   (action :initform
           (helm-make-actions
            "Open channel" #'helm-ypv-action-bookmark-open
            "Remove bookmark" #'helm-ypv-action-bookmark-remove))
   (migemo :initform t)))

(defvar helm-source-ypv-bookmarks
  (helm-make-source (helm-ypv-bookmark-add-source-mark "Bookmarks")
      'helm-ypv-bookmarks-source))


;;;; Provide
(provide 'helm-ypv-source-bookmark)
