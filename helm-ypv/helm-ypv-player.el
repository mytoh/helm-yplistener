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
                                      " &")))
    (message command)
    (start-process-shell-command "ypv" nil command)))


(cl-defun helm-ypv-player-mpv (url)
  (message url)
  (cl-letf ((command (seq-concatenate 'string
                                      "mpv "
                                      "'" url "'"
                                      " --ytdl=no --loop=inf"
                                      " &")))
    (message command)
    (start-process-shell-command "ypv" nil command)))

(provide 'helm-ypv-player)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
