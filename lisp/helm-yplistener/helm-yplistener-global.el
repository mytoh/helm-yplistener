;;; global.el  -*- lexical-binding: t -*-

;;;;; Requires
(require 'cl-lib) ; don't use cl.el
(require 'helm)
(require 'helm-utils)
(require 'url)
(require 'seq)

(require 'colle)
(require 'glof)

;;;;;; Local
(require 'helm-yplistener-user-variable "helm-yplistener/helm-yplistener-user-variable")

;;;;; Functions
(cl-defun helm-yplistener-make-yp-index-url (info)
  (seq-concatenate 'string "http://" info "/" "index.txt"))

(cl-defun helm-yplistener-parse-channels (info)
  (cl-letf* ((yp-name (car info))
             (content (cadr info))
             (channels (split-string content "\n")))
    (colle:map
     (lambda (x)
       (helm-yplistener-info->channel
        (seq-concatenate 'list
                         (list yp-name)
                         (split-string x "<>"))))
     channels)))

(cl-defun helm-yplistener-message (fmt &rest text)
  (apply #'message (format "[%s] %s"
                           (propertize "helm-yplistener"
                                       'face '(:foreground "#539b8f"))
                           fmt)
         text))

(cl-defun helm-yplistener-info->channel (info)
  (glof:plist
   :yp (helm-stringify (seq-elt info 0))
   :name (seq-elt info 1)
   :id (seq-elt info 2)
   :tracker (seq-elt info 3)
   :contact (seq-elt info 4)
   :genre (seq-elt info 5)
   :desc (seq-elt info 6)
   :bitrate (seq-elt info 9)
   :type (seq-elt info 10)
   :uptime (seq-elt info 16)
   :listeners (seq-elt info 7)
   :relays (seq-elt info 8)
   :comment (seq-elt info 18)))

(cl-defun helm-yplistener-replace-html-entities (str)
  (cl-letf ((ents '(("&lt;" "<")
                    ("&gt;" ">")
                    ("&quot;" "\"")
                    ("&#034;" "\"")
                    ("&#039;" "'")
                    ("&amp;" "&"))))
    (helm-yplistener-replace-html-entities-helper
     ents str)))

(cl-defun helm-yplistener-replace-html-entities-helper (lst str)
  (pcase lst
    (`nil str)
    (`((,target ,entity) . ,tail)
      (cl-letf* ((rep-str (replace-regexp-in-string
                           target entity str)))
        (helm-yplistener-replace-html-entities-helper
         tail rep-str)))))

(cl-defun helm-yplistener-url-retrieve (url)
  (helm-yplistener-message "get %s" url)
  (cl-letf ((res (helm-yplistener-remove-http-header
                  (url-retrieve-synchronously url 'silent 'inhibit-cookies))))
    (if (helm-yplistener-empty-response-p res)
        nil
      (helm-html-decode-entities-string res))))

(cl-defun helm-yplistener-get-channel (info)
  (if-let ((res (thread-first (cadr info)
                  helm-yplistener-make-yp-index-url
                  helm-yplistener-url-retrieve)))
      (list (car info) res)
    nil))

(cl-defun helm-yplistener-get-channels (yp-info)
  (colle:map
   #'helm-yplistener-get-channel
   yp-info))

(cl-defun helm-yplistener-remove-http-header (buf)
  ;; remove header info, [[frozenlock.org/2012/07/07/url-retrieve-and-json-api]]
  (cl-letf ((content nil))
    (with-current-buffer buf
      (save-excursion
        (goto-char (1+ url-http-end-of-headers))
        (setq content (buffer-substring-no-properties (point) (1- (point-max))))
        (kill-buffer (current-buffer))))
    (helm-yplistener-string->utf-8 content)))

(cl-defun helm-yplistener-empty-response-p (str)
  (if (stringp str)
      (string-match-p "^\n$" str)
    nil))

(cl-defun helm-yplistener-string->utf-8 (str)
  (decode-coding-string str 'utf-8-unix))

(cl-defun helm-yplistener-get/parse-channels (yp-infos)
  (cl-mapcan
   #'helm-yplistener-parse-channels
   (cl-remove nil (helm-yplistener-get-channels yp-infos))))


;;; Provide
(provide 'helm-yplistener-global)
