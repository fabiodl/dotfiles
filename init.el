;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    tangotango-theme
    py-autopep8))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'tangotango t) ;; load theme
(global-linum-mode t) ;; enable line numbers globally

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(elpy-use-ipython)


;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


(setq ein:use-auto-complete t)

(global-auto-complete-mode t)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional





(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)


(setq saved-cursor-color  (frame-parameter nil 'cursor-color))


 ;; Change cursor color according to mode
    (defvar hcz-set-cursor-color-color "")
    (defvar hcz-set-cursor-color-buffer "")
    (defun hcz-set-cursor-color-according-to-mode ()
      "change cursor color according to some minor modes."
      ;; set-cursor-color is somewhat costly, so we only call it when needed:
      (let ((color
             (if buffer-read-only "white"
               (if overwrite-mode "orange"
               (if mozc-mode "red"
;;                 saved-cursor-color
                 "SkyBlue"
)))))
        (unless (and
                 (string= color hcz-set-cursor-color-color)
                 (string= (buffer-name) hcz-set-cursor-color-buffer))
          (set-cursor-color (setq hcz-set-cursor-color-color color))
          (setq hcz-set-cursor-color-buffer (buffer-name)))))
    (add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1c50040ec3b3480b1fec3a0e912cac1eb011c27dd16d087d61e72054685de345" "b3380cb88fcc06ac42577ba9c2701e4cf0e5e60466b62ae94c76aabc132fdcdd" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" default)))
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly ring stamp track)))
 '(flymake-gui-warnings-enabled nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "SeaGreen4"))))
 '(erc-my-nick-face ((t (:foreground "royal blue" :weight bold))))
 '(erc-pal-face ((t (:foreground "light pink" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "sea green" :weight bold)))))


(set-face-attribute 'default nil
                        :background "black")

;;hide flycheck warnings
(set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
(set-face-attribute 'flycheck-warning nil :underline nil)

(require 'socks)

(when (load "erc" t)
(setq erc-server "irc.freenode.net")

;;(setq erc-port "6697") ;;enable this for tls
(setq erc-nick "Nick")
(setq erc-pals '("pal"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-notify-list '("pal"))
(setq erc-interpret-mirc-color t)
;;for proxy
(setq erc-server-connect-function 'socks-open-network-stream)
(setq socks-server (list "cyg" "localhost" 8080 5))
)

(require 'appt)
(setq appt-message-warning-time 3)      ; 0 minute time before warning
;(setq diary-file "~/diary")             ; diary file
(setq appt-display-diary nil)            ;do not show the full diary when starting emacs
(setq appt-display-mode-line t)
(appt-activate 1)


(defun close-if-exists (name)
"close window if existing"
  (ignore-errors (delete-windows-on name ))
)

(defun erc-closeall ()
  (mapcar 'close-if-exists '("pal0" "pal1") )
)

(defun toggle-buffer-visibility (buffername)
  "toggle "
  (if (get-buffer-window buffername) 
    (progn
      (ignore-errors (delete-windows-on buffername ))
     )    
    (progn
      (split-window-below)
      (switch-to-buffer-other-window buffername      )
      ) 
    )
  )
  


(global-set-key (kbd "<f5>") (lambda() (interactive) (toggle-buffer-visibility "pal0")))
(global-set-key (kbd "M-<f5>") (lambda() (interactive) (toggle-buffer-visibility "pal1")))
(global-set-key (kbd "S-<f5>") (lambda() (interactive) (toggle-buffer-visibility "irc.freenode.net:6667")))
(global-set-key (kbd "<f9>") (lambda() (interactive) (erc-closeall)))

(set-frame-parameter (selected-frame) 'alpha '(80 . 60))
(add-to-list 'default-frame-alist '(alpha . (80 . 60)))


(setq mouse-autoselect-window t)
(menu-bar-mode 1)
(global-unset-key (kbd "C-z"))

(server-start)