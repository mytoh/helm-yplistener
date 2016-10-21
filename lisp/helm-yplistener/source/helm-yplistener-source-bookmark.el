;;; bookmark.el -*- lexical-binding: t -*-

;;;; Requires
(require 'cl-lib) ; don't use cl.el (require 'helm)
(require 'seq)
(require 'pp)

(require 'colle)
(require 'glof)

;;;;; Local
(require 'helm-yplistener-user-variable "helm-yplistener/helm-yplistener-user-variable")
(require 'helm-yplistener-player "helm-yplistener/helm-yplistener-player")
(require 'helm-yplistener-face "helm-yplistener/helm-yplistener-face")
(require 'helm-yplistener-url "helm-yplistener/helm-yplistener-url")

(autoload 'helm-yplistener-get/parse-channels "helm-yplistener/helm-yplistener-global")
(autoload 'helm-yplistener-player "helm-yplistener/helm-yplistener-global")


;;;; Functions
;;;;; Utils

(cl-defun helm-yplistener-bookmark-equal-id (bmk1 bmk2)
  (equal (glof:get bmk1 :id)
         (glof:get bmk2 :id)))

(cl-defun helm-yplistener-bookmark-equal-name (bmk1 bmk2)
  (equal (glof:get bmk1 :name)
         (glof:get bmk2 :name)))

(cl-defun helm-yplistener-bookmark-remove-if-name (new-bookmark bookmarks)
  (seq-remove (lambda (old-bookmark)
                (helm-yplistener-bookmark-equal-name
                 old-bookmark new-bookmark))
              bookmarks))

;;;;; Bookmark Data
(cl-defun helm-yplistener-bookmark-data-write (file data)
  (with-temp-file file (cl-letf ((standard-output (current-buffer)))
                         (pp data))))

(cl-defun helm-yplistener-bookmark-data-add (bookmark file)
  (if (file-exists-p file)
      (if (helm-yplistener-bookmark-data-channel-exists-p bookmark file)
          (helm-yplistener-bookmark-data-update bookmark file)
        (helm-yplistener-bookmark-data-append bookmark file))
    (helm-yplistener-bookmark-data-write file (list bookmark))))

(cl-defun helm-yplistener-bookmark-data-append (bookmark file)
  (cl-letf ((old (helm-yplistener-bookmark-data-read file)))
    (message "updating bookmark")
    (thread-last bookmark
      list
      (seq-concatenate 'list old)
      (helm-yplistener-bookmark-data-write file))
    (message (format "added %s" bookmark))))

(cl-defun helm-yplistener-bookmark-data-update (bookmark file)
  (cl-letf ((new (thread-last file
                   helm-yplistener-bookmark-data-read
                   (helm-yplistener-bookmark-remove-if-name bookmark))))
    (if (helm-yplistener-bookmark-data-channel-exists-p bookmark file)
        (cl-locally (message "updating bookmark")
                    (helm-yplistener-bookmark-data-write file (seq-concatenate 'list new (list bookmark)))
                    (message (format "update to add %s" bookmark)))
      (message "channel not found on bookmark"))))

(cl-defun helm-yplistener-bookmark-data-remove (bookmark file)
  (cl-letf ((old (helm-yplistener-bookmark-data-read file)))
    (message "removing bookmark")
    (helm-yplistener-bookmark-data-write file
                                         (helm-yplistener-bookmark-remove-if-name
                                          bookmark old))
    (message (format "removed %s" bookmark))))

(cl-defun helm-yplistener-bookmark-data-channel-exists-p (bkm file)
  (cl-find bkm (helm-yplistener-bookmark-data-read file)
           :test #'helm-yplistener-bookmark-equal-name))

(cl-defun helm-yplistener-bookmark-data-read (file)
  (with-temp-buffer (insert-file-contents file)
                    (read (current-buffer))))

(cl-defun helm-yplistener-bookmark-data-file ()
  (expand-file-name
   helm-yplistener-bookmark-file-name
   (expand-file-name
    helm-yplistener-bookmark-directory-name
    user-emacs-directory)))


(cl-defun helm-yplistener-bookmark-channel->bookmark (channel)
  (glof:let (((yp :yp)
              (name :name)
              (id :id)
              (tracker :tracker)
              (contact :contact)
              (type :type))
             channel)
    (glof:plist :yp yp
                :name name
                :id id
                :tracker tracker
                :contact contact
                :type type
                :broadcasting nil)))

(cl-defun helm-yplistener-ensure-bookmark-directory (path)
  (unless (file-exists-p path)
    (make-directory path)))


;;;;; Action
(cl-defun helm-yplistener-action-bookmark-add (channel)
  (helm-yplistener-ensure-bookmark-directory
   (file-name-directory (helm-yplistener-bookmark-data-file)))
  (thread-first channel
    helm-yplistener-bookmark-channel->bookmark
    (helm-yplistener-bookmark-data-add (helm-yplistener-bookmark-data-file))))

(cl-defun helm-yplistener-action-bookmark-remove (bookmark)
  (helm-yplistener-bookmark-data-remove bookmark (helm-yplistener-bookmark-data-file)))

(cl-defun helm-yplistener-action-bookmark-open (bookmark)
  (cl-letf ((url (helm-yplistener-make-url `[:bookmark ,bookmark])))
    (helm-yplistener-bookmark-data-update bookmark (helm-yplistener-bookmark-data-file))
    (helm-yplistener-player helm-yplistener-player-type url)))

;;;;; Candidate
(cl-defun helm-yplistener-bookmark-create-display-candidate (bookmark)
  (cl-letf ((format-string "%-17.17s %s")
            (name (helm-yplistener-add-face (glof:get bookmark :name)
                                            (if (glof:get bookmark :broadcasting)
                                                'helm-yplistener-name
                                              'font-lock-comment-face)))
            (id (helm-yplistener-add-face (glof:get bookmark :id) 'helm-yplistener-id)))
    (format format-string
            name
            id)))

(cl-defun helm-yplistener-bookmark-channel-broadcasting-p (bookmark channels)
  (cl-find-if (lambda (channel)
                (equal (glof:get bookmark :id)
                       (glof:get channel :id)))
              channels))

(cl-defun helm-yplistener-bookmark-find-broadcasting-channels (bookmarks channels)
  (seq-remove #'null
              (colle:map (lambda (bookmark)
                        (if (helm-yplistener-bookmark-channel-broadcasting-p bookmark channels)
                            (helm-yplistener-bookmark-set-broadcasting bookmark t)
                          (helm-yplistener-bookmark-set-broadcasting bookmark nil)))
                      bookmarks)))

(cl-defun helm-yplistener-bookmark-set-broadcasting (obj value)
  (glof:assoc obj
              :broadcasting value))

(cl-defun helm-yplistener-bookmark-create-candidates (channels)
  (if (not (file-exists-p (helm-yplistener-bookmark-data-file)))
      '()
    (seq-map (lambda (bookmark)
               (cons
                ;; display candidate
                (helm-yplistener-bookmark-create-display-candidate bookmark)
                ;; real candidate
                bookmark))
             (helm-yplistener-bookmark-find-broadcasting-channels
              (helm-yplistener-bookmark-data-read (helm-yplistener-bookmark-data-file))
              channels))))

;;;;; Source
(defvar helm-yplistener-candidate-bookmarks nil)

(cl-defun helm-yplistener-bookmark-init ()
  (setq helm-yplistener-candidate-bookmarks
        (helm-yplistener-bookmark-create-candidates
         (helm-yplistener-get/parse-channels helm-yplistener-yp-urls))))

(defun helm-yplistener-bookmark-add-source-mark (name)
  (cl-letf ((mark "🔖")) ; "\U0001F516"
    (cond ((window-system)
           (seq-concatenate 'string " " mark " "  name))
          (t
           name))))

(defclass helm-yplistener-bookmarks-source (helm-source-sync)
  ((init  :initform #'helm-yplistener-bookmark-init)
   (candidates :initform 'helm-yplistener-candidate-bookmarks)
   (action :initform
     (helm-make-actions
      "Open channel" #'helm-yplistener-action-bookmark-open
      "Remove bookmark" #'helm-yplistener-action-bookmark-remove))
   (migemo :initform t)))

(defvar helm-source-yplistener-bookmarks
  (helm-make-source (helm-yplistener-bookmark-add-source-mark "Bookmarks")
      'helm-yplistener-bookmarks-source))


;;;; Provide
(provide 'helm-yplistener-source-bookmark)
