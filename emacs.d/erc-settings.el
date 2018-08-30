(require 'socks)

(when (load "erc" t)
(setq erc-server "irc.freenode.net")
;;(setq erc-port "6697") ;;enable this for tls
(setq erc-nick "nick")
(setq erc-pals '("pal"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-notify-list '("pal"))
(setq erc-interpret-mirc-color t)
;;for proxy
(setq erc-server-connect-function 'socks-open-network-stream)
(setq socks-server (list "cyg" "localhost" 8080 5))
)

(setq fast-close-buffers '("irc.freenode.org" "pal"))
(setq fast-toggle-buffers '(("<f5>" "pal")
                            ("M-<f5>" "pal_")
                            ("S-<f5>" "irc.freenode.net:6667")))
