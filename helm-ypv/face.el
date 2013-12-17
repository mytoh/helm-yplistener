
;;; face.el

;;;;; Faces

(cl-defun helm-ypv-add-face (str face)
  (propertize str 'face face))

(defface helm-ypv-name-face
  '((t :inherit font-lock-type-face))
  "face for channel name"
  :group 'helm-ypv)

(defface helm-ypv-genre-face
  '((t :inherit font-lock-keyword-face))
  "face for channel genre"
  :group 'helm-ypv)

(defface helm-ypv-desc-face
  '((t :inherit font-lock-string-face))
  "face for channel description"
  :group 'helm-ypv)

(defface helm-ypv-contact-face
  '((t :inherit font-lock-variable-name-face))
  "face for channel contact url"
  :group 'helm-ypv)

(defface helm-ypv-type-face
  '((t :inherit font-lock-type-face))
  "face for channel type"
  :group 'helm-ypv)

(defface helm-ypv-bitrate-face
  '((t :inherit font-lock-preprocessor-face))
  "face for channel bitrate"
  :group 'helm-ypv)

(defface helm-ypv-time-face
  '((t :inherit font-lock-preprocessor-face))
  "face for channel time"
  :group 'helm-ypv)

(defface helm-ypv-comment-face
  '((t :inherit font-lock-doc-face))
  "face for channel comment"
  :group 'helm-ypv)

(defface helm-ypv-id-face
  '((t :inherit font-lock-function-name-face))
  "face for channel id"
  :group 'helm-ypv)

;;;;; Provide
(provide 'helm-ypv-face)
