;;; helm-ypv.el --- yp viewer with helm

;;; Commentary:

;;; Code:

;;;; helm-ypv

;;;; deps
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)

;;;; local library
(require 'helm-ypv-global "helm-ypv/global")
(require 'helm-ypv-bookmark "helm-ypv/source/bookmark")
(require 'helm-ypv-channel "helm-ypv/source/channel")


;;;###autoload
(cl-defun helm-ypv ()
  "Preconfigured `helm' for Channels and Bookmarks"
  (interactive)
  (helm :sources '(helm-source-ypv-channels
                   helm-source-ypv-bookmarks)
        :buffer "*Helm ypv*"))

;;;###autoload
(cl-defun helm-ypv-bookmarks ()
  "Preconfigured `helm' for Yellow Pages bookmarks"
  (interactive)
  (helm :sources '(helm-source-ypv-bookmarks)
        :buffer "*Helm Ypv Bookmarks*"))

;;;###autoload
(cl-defun helm-ypv-channels ()
  "Preconfigured `helm' for Yellow Pages"
  (interactive)
  (helm :sources '(helm-source-ypv-channels)
        :buffer "*Helm Ypv Channels*"))

;;;; provide
(provide 'helm-ypv)

;;; helm-ypv.el ends here
