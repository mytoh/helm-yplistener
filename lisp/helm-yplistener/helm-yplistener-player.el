;;; player.el -*- lexical-binding: t -*-

(cl-defun helm-yplistener-player (player url)
  (pcase player
    (:mplayer2
     (helm-yplistener-player-mplayer2 url))
    (:mpv
     (helm-yplistener-player-mpv url))
    (:mplayer
     (helm-yplistener-player-mplayer url))))

(cl-defun helm-yplistener-player-mplayer (url)
  (message url)
  (cl-letf ((command (seq-concatenate 'string
                                      "mplayer "
                                      "'" url "'"
                                      " -softvol -softvol-max 300 -volume 40 -loop 0 -autosync 1 -nocache -really-quiet -noconsolecontrols "
                                      " &")))
    (message command)
    (start-process-shell-command "yplistener" nil command)))

(cl-defun helm-yplistener-player-mplayer2 (url)
  (message url)
  (cl-letf ((command (seq-concatenate 'string
                                      "mplayer "
                                      "'" url "'"
                                      " --softvol --autosync=1 --nocache --framedrop --really-quiet --no-consolecontrols --use-filename-title"
                                      " --zoom "
                                      " &")))
    (message command)
    (start-process-shell-command "yplistener" nil command)))


(cl-defun helm-yplistener-player-mpv (url)
  (message url)
  (cl-letf ((command (seq-concatenate 'string
                                      "mpv "
                                      (pcase url
                                        ((rx "twitch") 
                                         " --ytdl=yes --ytdl-format=Low ")
                                        (_ "--ytdl=no"))
                                      " --speed 1 --loop=inf "
                                      " --force-window=immediate "
                                      " --no-cache "
                                      ;; " -{ av://lavfi:color -length 1 -} "
                                      "'" url "'"
                                      " &")))
    (message command)
    (start-process-shell-command "yplistener" nil command)))

(provide 'helm-yplistener-player)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
