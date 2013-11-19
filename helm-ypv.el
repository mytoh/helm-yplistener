;;; helm-ypv.el --- yp viewer with helm

;;; Commentary:

;;; Code:


;;;; helm-ypv

;;;; deps
(eval-when-compile (require 'cl-lib)) ; don't use cl.el
(require 'helm)
(require 'dash)
(require 's)

(require 'helm-ypv-global)
(require 'helm-ypv-bookmark)
(require 'helm-ypv-channel)


;;;###autoload
(cl-defun helm-ypv ()
  "Preconfigured `helm' for Yellow Pages"
  (interactive)
  (helm :sources '(helm-source-ypv-channels
                   helm-source-ypv-bookmarks)
        :buffer "*Helm ypv*"))

;;;###autoload
(cl-defun helm-ypv-bookmarks ()
  "Preconfigured `helm' for Yellow Pages bookmarks"
  (interactive)
  (helm :sources '(helm-source-ypv-bookmarks)
        :buffer "*Helm ypv bookmarks*"))

;;;###autoload
(cl-defun helm-ypv-channels ()
  "Preconfigured `helm' for Yellow Pages"
  (interactive)
  (helm :sources '(helm-source-ypv-channels)
        :buffer "*Helm ypv*"))

;;;; provide
(provide 'helm-ypv)

;;; helm-ypv.el ends here
