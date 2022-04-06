;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))



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
    google-translate
    ercn
    csv-mode
    php-mode
    auctex
))

(defun install-missing-packages (package-list)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        package-list))

(install-missing-packages myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ; hide the startup message

(load-theme 'tangotango t)
(require 'better-defaults)
;background color
(defun set-new-frame-colors (frame)
  (if (window-system frame)
      (set-face-background 'default "black" frame)
      (set-face-background 'default "unspecified-bg" frame)))
;;these hooks are for server mode and standalone mode, respectively
(add-hook 'after-make-frame-functions 'set-new-frame-colors)
(add-hook 'window-setup-hook '(lambda() (set-new-frame-colors (selected-frame))))
(set-new-frame-colors (selected-frame))


;(global-linum-mode t) ; enable line numbers globally
(add-hook 'prog-mode-hook 'linum-mode)

(setq column-number-mode t) ;column number

(setq mouse-autoselect-window t)  ;sloppy focus
(setq verilog-linter "verilator --lint-only")

;;server
(require 'server)
(unless (server-running-p) (server-start))

(global-unset-key (kbd "C-z"))
(scroll-bar-mode t)
(menu-bar-mode t)

;;set transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 60))
(add-to-list 'default-frame-alist '(alpha . (100 . 60)))

;;allow the same file in multiple frames
(setq ido-default-buffer-method 'selected-window)


;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
;(elpy-use-ipython)
(setq python-shell-interpreter "ipython3"
      elpy-rpc-python-command "python3"
      python-shell-interpreter-args "--simple-prompt -i"
      elpy-shell-echo-input nil
      elpy-shell-starting-directory (quote current-directory) ; this makes C-c C-c run in the current dir
      elpy-shell-display-buffer-after-send t)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq ein:use-auto-complete t)
(setq ein:completion-backend 'ein:use-ac-jedi-backend)
(add-hook 'ein:notebook-mode-hook 'ein:remove-movecell-keybindings)

(defun ein:remove-movecell-keybindings ()
  (progn (define-key ein:notebook-mode-map (kbd "C-c <up>") nil)
         (define-key ein:notebook-mode-map (kbd "C-c <down>") nil)
         ))
(global-auto-complete-mode t)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

;;hide flycheck warnings
;(set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
(set-face-attribute 'flycheck-warning nil :underline nil)
(setq flycheck-highlighting-mode 'lines)

;fix ein authentication bug
(advice-add 'request--netscape-cookie-parse :around #'fix-request-netscape-cookie-parse)

;;make transparent figures visible
(defadvice ein:insert-image (around ein-transparent-color-replacement activate)
  (ad-set-args 0 (append (ad-get-args 0) `(:background ,(face-attribute 'ein:cell-output-area :background))  )) ad-do-it)

(defadvice ein:cell-append-display-data (around ein-transparent-color-replacement activate)
  (ein:insert-read-only  "\n ")
  ad-do-it
  (ein:insert-read-only "\n")
  )


;; JAPANESE
;; --------------------------------------

(require 'mozc)
;; or (load-file "/path/to/mozc.el")
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(load-file "~/.emacs.d/mozc-isearch.el")



(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist '(("ja" . "en")("en" . "ja")) ;you can reverse by C-n C-p
      google-translate-show-phonetic t
      )

(defun google-translate--get-b-d1 ()
    ;; TKK='427110.1469889687'
  (list 427110 1469889687))


;from https://github.com/atykhonov/google-translate/issues/98#issuecomment-562870854
 (defun google-translate-json-suggestion (json)
  "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
  (let ((info (aref json 7)))
    (if (and info (> (length info) 0))
        (aref info 1)
      nil)))                       



(add-to-list 'face-ignored-fonts "Noto Serif CJK")
(add-to-list 'face-ignored-fonts "Noto Sans CJK")


;; WINDOWS NAVIGATION
;; --------------------------------------

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)



;; CURSOR COLOR
;; --------------------------------------

;(setq saved-cursor-color  (frame-parameter nil 'cursor-color))

;; Change cursor color according to mode
(defun get-my-cursor-color()
  (cond (buffer-read-only "white")
        (overwrite-mode "orange")
        (mozc-mode "red")
        (t "SkyBlue") ;saved-cursor-color
   )     
  )
(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color (get-my-cursor-color) ))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
(add-hook 'focus-in-hook '(lambda() (set-cursor-color (get-my-cursor-color))))


;; TITLE
;; --------------------------------------

(setq frame-title-format '(multiple-frames "emacs %b"
                 ("" invocation-name "@" system-name)))


;; DIARY
;; --------------------------------------


(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(setq calendar-mark-diary-entries-flag t)

(defun optConcat (x)
  (if (listp x) (string-join x ", ") x))
 
(require 'appt)
(require 'notifications)
(defun my-appt-display (time-to-event curr-time message)
  (notifications-notify :title (optConcat message)
                       :body (format "In %s minutes" (optConcat time-to-event))
                       :app-name "Emacs: Org"
                       :sound-name "alarm-clock-elapsed"
                       :transient t
                       :timeout (if (string= "0" (optConcat time-to-event))  1800000 -1)
                      )
  (appt-disp-window time-to-event curr-time message)
 )

(setq appt-disp-window-function 'my-appt-display)

(setq appt-message-warning-time 12       ;from how much before
      appt-display-interval 3            ;interval betwen reminders
      appt-display-diary nil             ;do not show the full diary when starting emacs
      appt-display-mode-line t
      )      

(appt-activate t)

;; TOGGLE WINDOWS
;; --------------------------------------

(defvar fast-close-buffers nil "buffers to close with the defined key")
(defvar fast-toggle-buffers nil "a list of pairs key-buffername")

(defun close-if-exists (name)
"close window if existing"
  (ignore-errors (delete-windows-on name ))
)

(defun closeall-fast-close-buffers()
  (mapcar 'close-if-exists fast-close-buffers)
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
      (switch-to-buffer-other-window buffername)
      ) 
    )
  )

;https://emacs.stackexchange.com/questions/10394/scope-in-lambda
(defun assign-key-to-buffer-toggle (key buffername)
  (global-set-key (kbd key) `(lambda() (interactive) (toggle-buffer-visibility ,buffername))))


(defun load-file-when-existing(fname)
  "loads the file if it exists" 
  (when (file-exists-p fname)
    (load-file fname)))

(load-file-when-existing "~/.emacs.d/erc-settings.el")
;;snippets from http://home.thep.lu.se/~karlf/emacs.html

(defun erc-ignore-unimportant (msg)
  (if (or (string-match "*** localhost has changed mode for &bitlbee to" msg)
          (string-match "<root> discord - Remote host is closing websocket connection" msg)
          (string-match "<root> discord - Performing soft-reconnect" msg)
          (string-match "Account already online" msg))
      (setq erc-insert-this nil)))
(add-hook 'erc-insert-pre-hook 'erc-ignore-unimportant)

(setq erc-auto-query 'bury ;prevent queries from opening new windows
      erc-server-flood-penalty 0
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      erc-track-exclude '("&bitlbee" "localhost:6667")
      erc-accidental-paste-threshold-seconds nil
      erc-warn-about-blank-lines nil
      ) 

(require 'ercn)
(setq ercn-notify-rules '((query-buffer . all)
                          (message . ("#general"))
                          ))

(defun dbus-erc-notify (nickname message)
 ""
 (notifications-notify :title "erc"
                       :transient t
                       :body (substring nickname 0 1)
                       :timeout 5000
                      ))
(add-hook 'ercn-notify-hook 'dbus-erc-notify)

(mapcar #'(lambda(pair) (assign-key-to-buffer-toggle (car pair) (cadr pair) )) fast-toggle-buffers)  
(global-set-key (kbd "<f9>") (lambda() (interactive) (closeall-fast-close-buffers)))


;;EWW
(require 'eww)
(defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page fro cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

(define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
(define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

;; minimal rendering by default
(setq-default shr-inhibit-images t)   ; toggle with `I`
(setq-default shr-use-fonts nil)      ; toggle with `F`


;; CODING
;; --------------------------------------

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; For folding
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

(define-key global-map (kbd "C-;") 'hs-toggle-hiding)


;Z80
(load "~/.emacs.d/z80-mode.el")
(add-to-list 'auto-mode-alist '("\\.asm\\'" . z80-mode))
(add-to-list 'auto-mode-alist '("\\.i\\'" . z80-mode))
(add-to-list 'ac-modes 'z80-mode)
(setq-default indent-tabs-mode nil)

(setq hexl-bits 8)

;;colorized compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)



;; WORK RELATED
;; --------------------------------------
(load "~/.emacs.d/work.el")

(if (not window-system)		;; Only use in tty-sessions.
     (progn
      (defvar arrow-keys-map (make-sparse-keymap) "Keymap for arrow keys")
      (define-key esc-map "[" arrow-keys-map)
      (define-key arrow-keys-map "A" 'previous-line)
      (define-key arrow-keys-map "B" 'next-line)
      (define-key arrow-keys-map "C" 'forward-char)
      (define-key arrow-keys-map "D" 'backward-char)))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-output-area ((t (:background "gray40" :foreground "black"))) t)
 '(erc-input-face ((t (:foreground "SeaGreen4"))))
 '(erc-my-nick-face ((t (:foreground "royal blue" :weight bold))))
 '(erc-pal-face ((t (:foreground "light pink" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "sea green" :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-hello-auto-refresh t)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i" :search-type tree)
     (:name "unread" :query "tag:unread" :key "u" :search-type tree)
     (:name "flagged" :query "tag:flagged" :key "f" :search-type tree)
     (:name "sent" :query "tag:Sent" :key "t" :search-type tree)
     (:name "drafts" :query "tag:draft" :key "d" :search-type tree)
     (:name "all mail" :query "*" :key "a" :search-type tree))))
 '(notmuch-tag-formats
   (quote
    (("unread"
      (propertize tag
                  (quote face)
                  (quote notmuch-tag-unread)))
     ("flagged"
      (notmuch-tag-format-image-data tag
                                     (notmuch-tag-star-icon))
      (propertize tag
                  (quote face)
                  (quote notmuch-tag-flagged)))
     ("amro/toConfirm"
      (notmuch-apply-face tag
                          (quote
                           (:foreground "magenta")))))))
 '(package-selected-packages
   (quote
    (yaml-mode telega csv-mode php-mode auctex mozc-im japanese-holidays w3m ercn google-translate mwim haskell-mode magit jedi mozc flycheck-pyflakes py-autopep8 tangotango-theme flycheck elpy ein better-defaults)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "localhost")
 '(smtpmail-smtp-service 1025)
 '(telega-mode-line-string-format
   (quote
    ("   "
     (:eval
      (when telega-use-tracking-for
        (telega-mode-line-tracking)))
     (:eval
      (telega-mode-line-unread-unmuted))
     (:eval
      (telega-mode-line-mentions
       (quote messages))))))
 '(telega-notifications-mode t)
 '(telega-notifications-msg-body-limit 0))

