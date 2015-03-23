;;; player.el -*- lexical-binding: t -*-

(cl-defun helm-ypv-player (player url)
  (pcase player
    ('mplayer2
     (helm-ypv-player-mplayer2 url))
    ('mpv
     (helm-ypv-player-mpv url))))

(cl-defun helm-ypv-player-mplayer2 (url)
  (message url)
  (cl-letf ((command (seq-concatenate 'string
                                      "mplayer "
                                      "'" url "'"
                                      " --softvol --autosync=1 --nocache --framedrop --really-quiet --no-consolecontrols --use-filename-title"
                                      " --zoom "
                                      " &")))
    (message command)
    (start-process-shell-command "ypv" nil command)))


(cl-defun helm-ypv-player-mpv (url)
  (message url)
  (cl-letf ((command (seq-concatenate 'string
                                      "mpv "
                                      "--ytdl=no --loop=inf "
                                      ;; " -{ av://lavfi:color -length 1 -} "
                                      "'" url "'"
                                      " &")))
    (message command)
    (start-process-shell-command "ypv" nil command)))

(provide 'helm-ypv-player)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
