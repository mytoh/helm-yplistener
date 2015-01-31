;;; helm-ypv.el --- yp viewer with helm   -*- lexical-binding: t -*-

;;;; Commentary:

;;;; Code:
;;;;; Requires
;;;;;; General

(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)

;;;;;; Local library

(require 'helm-ypv-global "helm-ypv/helm-ypv-global")
(require 'helm-ypv-user-variable "helm-ypv/helm-ypv-user-variable")
(require 'helm-ypv-source-channel "helm-ypv/source/helm-ypv-source-channel")
(require 'helm-ypv-source-bookmark "helm-ypv/source/helm-ypv-source-bookmark")

;;;;; Autoloads
;;;###autoload
(cl-defun helm-ypv ()
  "Preconfigured `helm' for Channels and Bookmarks : [\\[helm-ypv]]"
  (interactive)
  (helm :sources '(helm-source-ypv-channels
                   helm-source-ypv-bookmarks)
        :buffer "*helm ypv*"))

;;;###autoload
(cl-defun helm-ypv-bookmarks ()
  "Preconfigured `helm' for Yellow Pages bookmarks : [\\[helm-ypv-bookmarks]]"
  (interactive)
  (helm :sources '(helm-source-ypv-bookmarks)
        :buffer "*helm ypv bookmarks*"))

;;;###autoload
(cl-defun helm-ypv-channels ()
  "Preconfigured `helm' for Yellow Pages : [\\[helm-ypv-channels]]"
  (interactive)
  (helm :sources '(helm-source-ypv-channels)
        :buffer "*helm ypv channels*"))

;;;;; Provide
(provide 'helm-ypv)

;;; helm-ypv.el ends here
