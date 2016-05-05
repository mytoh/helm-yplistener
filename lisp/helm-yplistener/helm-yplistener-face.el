;;; face.el  -*- lexical-binding: t -*-

;;;; Helper Function

(cl-defun helm-yplistener-add-face (str face)
  (if str
      (propertize str 'face face)
    ""))

;;;; Faces

(defface helm-yplistener-name
    '((t :inherit font-lock-type-face))
  "face for channel name"
  :group 'helm-yplistener)

(defface helm-yplistener-genre
    '((t :inherit font-lock-keyword-face))
  "face for channel genre"
  :group 'helm-yplistener)

(defface helm-yplistener-desc
    '((t :inherit font-lock-string-face))
  "face for channel description"
  :group 'helm-yplistener)

(defface helm-yplistener-contact
    '((t :inherit font-lock-variable-name-face))
  "face for channel contact url"
  :group 'helm-yplistener)

(defface helm-yplistener-type
    '((t :inherit font-lock-type-face))
  "face for channel type"
  :group 'helm-yplistener)

(defface helm-yplistener-bitrate
    '((t :inherit font-lock-preprocessor-face))
  "face for channel bitrate"
  :group 'helm-yplistener)

(defface helm-yplistener-uptime
    '((t :inherit font-lock-preprocessor-face))
  "face for channel uptime"
  :group 'helm-yplistener)

(defface helm-yplistener-comment
    '((t :inherit font-lock-doc-face))
  "face for channel comment"
  :group 'helm-yplistener)

(defface helm-yplistener-id
    '((t :inherit font-lock-function-name-face))
  "face for channel id"
  :group 'helm-yplistener)

(defface helm-yplistener-lr
    '((t :inherit font-lock-builtin-face))
  "face for channel listeners number"
  :group 'helm-yplistener)

;;;; Provide
(provide 'helm-yplistener-face)
