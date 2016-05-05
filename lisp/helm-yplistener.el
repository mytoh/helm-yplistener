;;; helm-yplistener.el --- yp viewer with helm   -*- lexical-binding: t -*-

;;;; Commentary:

;;;; Code:
;;;;; Requires
;;;;;; General

(require 'cl-lib) ; don't use cl.el
(require 'helm)

;;;;;; Local library

(require 'helm-yplistener-global "helm-yplistener/helm-yplistener-global")
(require 'helm-yplistener-user-variable "helm-yplistener/helm-yplistener-user-variable")
(require 'helm-yplistener-source-channel "helm-yplistener/source/helm-yplistener-source-channel")
(require 'helm-yplistener-source-bookmark "helm-yplistener/source/helm-yplistener-source-bookmark")

;;;;; Autoloads
;;;###autoload
(cl-defun helm-yplistener ()
  "Preconfigured `helm' for Channels and Bookmarks : [\\[helm-yplistener]]"
  (interactive)
  (helm :sources '(helm-source-yplistener-channels
                   helm-source-yplistener-bookmarks)
        :buffer "*helm yplistener*"))

;;;###autoload
(cl-defun helm-yplistener-bookmarks ()
  "Preconfigured `helm' for Yellow Pages bookmarks : [\\[helm-yplistener-bookmarks]]"
  (interactive)
  (helm :sources '(helm-source-yplistener-bookmarks)
        :buffer "*helm yplistener bookmarks*"))

;;;###autoload
(cl-defun helm-yplistener-channels ()
  "Preconfigured `helm' for Yellow Pages : [\\[helm-yplistener-channels]]"
  (interactive)
  (helm :sources '(helm-source-yplistener-channels)
        :buffer "*helm yplistener channels*"))

;;;;; Provide
(provide 'helm-yplistener)

;;; helm-yplistener.el ends here
