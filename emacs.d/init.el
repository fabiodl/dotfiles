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

(setq inhibit-startup-message t) ; hide the startup message

(load-theme 'tangotango t)

;background color
(defun set-new-frame-colors (frame)
  (if (window-system frame)
      (set-face-background 'default "black" frame)
      (set-face-background 'default "unspecified-bg" frame)))
;;these hooks are for server mode and standalone mode, respectively
(add-hook 'after-make-frame-functions 'set-new-frame-colors)
(add-hook 'window-setup-hook '(lambda() (set-new-frame-colors (selected-frame))))

(global-linum-mode t) ; enable line numbers globally
(setq column-number-mode t) ;column number

(setq mouse-autoselect-window t)  ;sloppy focus

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
(setq jedi:complete-on-dot t)                 ; optional

;;hide flycheck warnings
;(set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
(set-face-attribute 'flycheck-warning nil :underline nil)
(setq flycheck-highlighting-mode 'lines)

;;fix ein authentication bug
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

;; DIARY
;; --------------------------------------

(require 'appt)
(setq appt-message-warning-time 3)       ;minute time before warning
;(setq diary-file "~/diary")             ;diary file
(setq appt-display-diary nil)            ;do not show the full diary when starting emacs
(setq appt-display-mode-line t)
(appt-activate 1)

;; TOGGLE WINDOWS
;; --------------------------------------

(defvar fast-close-buffers "buffers to close with the defined key")
(defvar fast-toggle-buffers "a list of pairs key-buffername")

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
  (global-set-key (kbd key) `(lambda() (interactive) (toggle-buffer-visibility ,buffername)))
  )


(let ((erc-settings-file "~/.emacs.d/erc-settings.el"))
 (when (file-exists-p erc-settings-file)
   (load-file erc-settings-file))
)

(mapcar #'(lambda(pair) (assign-key-to-buffer-toggle (car pair) (cadr pair) )) fast-toggle-buffers)  
(global-set-key (kbd "<f9>") (lambda() (interactive) (closeall-fast-close-buffers)))




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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    (default)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly ring stamp track)))
 '(flymake-gui-warnings-enabled nil)
 '(package-selected-packages
   (quote
    (mwim magit tangotango-theme py-autopep8 mozc jedi flycheck-pyflakes elpy ein better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-output-area ((t (:background "LightSteelBlue1" :foreground "black"))) t)
 '(erc-input-face ((t (:foreground "SeaGreen4"))))
 '(erc-my-nick-face ((t (:foreground "royal blue" :weight bold))))
 '(erc-pal-face ((t (:foreground "light pink" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "sea green" :weight bold)))))

