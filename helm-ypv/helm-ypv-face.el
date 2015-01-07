;;; face.el  -*- lexical-binding: t -*-

;;;; Helper Function

(cl-defun helm-ypv-add-face (str face)
  (if str
      (propertize str 'face face)
    ""))

;;;; Faces

(defface helm-ypv-name
    '((t :inherit font-lock-type-face))
  "face for channel name"
  :group 'helm-ypv)

(defface helm-ypv-genre
    '((t :inherit font-lock-keyword-face))
  "face for channel genre"
  :group 'helm-ypv)

(defface helm-ypv-desc
    '((t :inherit font-lock-string-face))
  "face for channel description"
  :group 'helm-ypv)

(defface helm-ypv-contact
    '((t :inherit font-lock-variable-name-face))
  "face for channel contact url"
  :group 'helm-ypv)

(defface helm-ypv-type
    '((t :inherit font-lock-type-face))
  "face for channel type"
  :group 'helm-ypv)

(defface helm-ypv-bitrate
    '((t :inherit font-lock-preprocessor-face))
  "face for channel bitrate"
  :group 'helm-ypv)

(defface helm-ypv-uptime
    '((t :inherit font-lock-preprocessor-face))
  "face for channel uptime"
  :group 'helm-ypv)

(defface helm-ypv-comment
    '((t :inherit font-lock-doc-face))
  "face for channel comment"
  :group 'helm-ypv)

(defface helm-ypv-id
    '((t :inherit font-lock-function-name-face))
  "face for channel id"
  :group 'helm-ypv)

(defface helm-ypv-lr
    '((t :inherit font-lock-builtin-face))
  "face for channel listeners number"
  :group 'helm-ypv)

;;;; Provide
(provide 'helm-ypv-face)
