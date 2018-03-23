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
    py-autopep8
    flycheck-pyflakes
    auto-complete
    mozc
    jedi
    magit
    haskell-mode
    mwim
))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message

(if (display-graphic-p) 
    (load-theme 'tangotango t) 
)

(global-linum-mode t) ;; enable line numbers globally
(setq column-number-mode t) ;;column number

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(elpy-use-ipython)
(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt -i")

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
;(setq jedi:complete-on-dot t)                 ; optional





(require 'mozc)
;; or (load-file "/path/to/mozc.el")
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
 '(custom-safe-themes
   (quote
    ("aee1f5a2825f6752eb447c0d1b480b672c354adf3b012f607a753ad09cc9b942" "1c50040ec3b3480b1fec3a0e912cac1eb011c27dd16d087d61e72054685de345" "b3380cb88fcc06ac42577ba9c2701e4cf0e5e60466b62ae94c76aabc132fdcdd" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" default)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly ring stamp track)))
 '(flymake-gui-warnings-enabled nil)
 '(package-selected-packages
   (quote
    (haskell-mode magit tangotango-theme py-autopep8 mozc jedi flycheck-pyflakes elpy ein better-defaults))))
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
;(set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
(set-face-attribute 'flycheck-warning nil :underline nil)
(setq flycheck-highlighting-mode 'lines)
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

(require 'appt)
(setq appt-message-warning-time 3)      ; 0 minute time before warning
;(setq diary-file "~/diary")             ; diary file
(setq appt-display-diary nil)            ;do not show the full diary when starting emacs
(setq appt-display-mode-line t)
(appt-activate 1)



;(global-set-key (kbd "<f6>") (lambda()(if (get-buffer-window "nonnatropicale" visible) (message "hello") (lambda() 
;                               (interactive)(split-window-below)
;                               (interactive)(switch-to-buffer-other-window "nonnatropicale" ))))
;)

(defun close-if-exists (name)
"close window if existing"
  (ignore-errors (delete-windows-on name ))
)

(defun erc-closeall ()
  (mapcar 'close-if-exists '("win1" "win2") )
)

(defun toggle-buffer-visibility (buffername)
  "toggle "
  (if (get-buffer-window buffername) 
    (progn
      (ignore-errors (delete-windows-on buffername ))
     )    
    (progn
      (split-window-below)
      ; (switch-to-buffer buffername)
      (switch-to-buffer-other-window buffername      )
      ) 
    )
  )
  


(global-set-key (kbd "<f5>") (lambda() (interactive) (toggle-buffer-visibility "pal")))
(global-set-key (kbd "M-<f5>") (lambda() (interactive) (toggle-buffer-visibility "pal_")))
(global-set-key (kbd "S-<f5>") (lambda() (interactive) (toggle-buffer-visibility "irc.freenode.net:6667")))
(global-set-key (kbd "<f9>") (lambda() (interactive) (erc-closeall)))
(global-set-key (kbd "<muhenkan>") (lambda() (interactive) (erc-closeall)))



(set-frame-parameter (selected-frame) 'alpha '(100 . 60))
(add-to-list 'default-frame-alist '(alpha . (100 . 60)))


(setq mouse-autoselect-window t)
(menu-bar-mode 1)

(server-start)

(global-unset-key (kbd "C-z"))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(scroll-bar-mode 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For folding
;;;
(require 'hideshow)
;; C coding style
(add-hook 'c-mode-hook
          '(lambda ()
	    (hs-minor-mode 1)))
(add-hook 'c++-mode-hook
	 '(lambda ()
	    (hs-minor-mode 1)))
;; Scheme coding style
(add-hook 'scheme-mode-hook
          '(lambda ()
	    (hs-minor-mode 1)))
;; Elisp coding style
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
	    (hs-minor-mode 1)))
;; Lisp coding style
(add-hook 'lisp-mode-hook
          '(lambda ()
	    (hs-minor-mode 1)))
;; Python coding style
(add-hook 'python-mode-hook
          '(lambda ()
	    (hs-minor-mode 1)))
;; 
(define-key
  global-map
  (kbd "C-;") 'hs-toggle-hiding)

;for same file in multiple frames
(setq ido-default-buffer-method 'selected-window)

                                        ;Z80
(load "~/.emacs.d/z80-mode.el")
(add-to-list 'auto-mode-alist '("\\.asm\\'" . z80-mode))

(setq-default indent-tabs-mode nil)
