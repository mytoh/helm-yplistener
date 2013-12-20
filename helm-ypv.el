;;; helm-ypv.el --- yp viewer with helm   -*- lexical-binding: t -*-

;;;; Commentary:

;;;; Code:
;;;;; Requires
;;;;;; General

(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(autoload 'helm "helm")

;;;;;; Local library

(require 'helm-ypv-global "helm-ypv/global")
(require 'helm-ypv-user-variable "helm-ypv/user-variable")
(require 'helm-ypv-source-channel "helm-ypv/source/channel")
(require 'helm-ypv-source-bookmark "helm-ypv/source/bookmark")

;;;;; Autoloads
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

;;;;; Provide
(provide 'helm-ypv)

;;; helm-ypv.el ends here
