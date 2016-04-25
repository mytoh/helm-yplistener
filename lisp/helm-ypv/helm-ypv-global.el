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
(require 'helm-ypv-user-variable "helm-ypv/helm-ypv-user-variable")

;;;;; Functions
(cl-defun helm-ypv-make-yp-index-url (info)
  (seq-concatenate 'string "http://" info "/" "index.txt"))

(cl-defun helm-ypv-parse-channels (info)
  (cl-letf* ((yp-name (car info))
             (content (cadr info))
             (channels (split-string content "\n")))
    (colle:map
     (lambda (x)
       (helm-ypv-info->channel
        (seq-concatenate 'list
                         (list yp-name)
                         (split-string x "<>"))))
     channels)))

(cl-defun helm-ypv-message (fmt &rest text)
  (apply #'message (format "[%s] %s"
                           (propertize "helm-ypv"
                                       'face '(:foreground "#539b8f"))
                           fmt)
         text))

(cl-defun helm-ypv-info->channel (info)
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

(cl-defun helm-ypv-replace-html-entities (str)
  (cl-letf ((ents '(("&lt;" "<")
                    ("&gt;" ">")
                    ("&quot;" "\"")
                    ("&#034;" "\"")
                    ("&#039;" "'")
                    ("&amp;" "&"))))
    (helm-ypv-replace-html-entities-helper
     ents str)))

(cl-defun helm-ypv-replace-html-entities-helper (lst str)
  (pcase lst
    (`nil str)
    (`((,target ,entity) . ,tail)
      (cl-letf* ((rep-str (replace-regexp-in-string
                           target entity str)))
        (helm-ypv-replace-html-entities-helper
         tail rep-str)))))

(cl-defun helm-ypv-url-retrieve (url)
  (helm-ypv-message "get %s" url)
  (cl-letf ((res (helm-ypv-remove-http-header
                  (url-retrieve-synchronously url 'silent 'inhibit-cookies))))
    (if (helm-ypv-empty-response-p res)
        nil
      (helm-html-decode-entities-string res))))

(cl-defun helm-ypv-get-channel (info)
  (if-let ((res (thread-first (cadr info)
                  helm-ypv-make-yp-index-url
                  helm-ypv-url-retrieve)))
      (list (car info) res)
    nil))

(cl-defun helm-ypv-get-channels (yp-info)
  (colle:map
   #'helm-ypv-get-channel
   yp-info))

(cl-defun helm-ypv-remove-http-header (buf)
  ;; remove header info, [[frozenlock.org/2012/07/07/url-retrieve-and-json-api]]
  (cl-letf ((content nil))
    (with-current-buffer buf
      (save-excursion
        (goto-char (1+ url-http-end-of-headers))
        (setq content (buffer-substring-no-properties (point) (1- (point-max))))
        (kill-buffer (current-buffer))))
    (helm-ypv-string->utf-8 content)))

(cl-defun helm-ypv-empty-response-p (str)
  (if (stringp str)
      (string-match-p "^\n$" str)
    nil))

(cl-defun helm-ypv-string->utf-8 (str)
  (decode-coding-string str 'utf-8-unix))

(cl-defun helm-ypv-get/parse-channels (yp-infos)
  (cl-mapcan
   #'helm-ypv-parse-channels
   (cl-remove nil (helm-ypv-get-channels yp-infos))))


;;; Provide
(provide 'helm-ypv-global)