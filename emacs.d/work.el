(defvar corp-packages
  '(w3m
    wanderlust
    japanese-holidays)
  )

(install-missing-packages corp-packages)


(defvar corp-domain nil)
(defvar corp-username nil)
(defvar corp-mail nil)
(defvar corp-mail-from nil)
(defvar corp-smb-domain nil)
(defvar corp-smb-format nil)
(load-file-when-existing "~/.emacs.d/corp-settings.el")

;; CALENDAR
;; --------------------------------------

(defun update-calendar-from-url (url diaryfile)
  "write the remote calendar to diaryfile. The original diaryfile is lost"
  (let ((dstfile))
    (setq dstfile (expand-file-name diaryfile))    
    (write-region "" nil dstfile) ;clear the dstfile
    (with-temp-buffer
      (url-insert-file-contents url)
      (icalendar-import-buffer dstfile t)
      )
    (kill-buffer (get-file-buffer diaryfile)) ;close the dstfile buffer
    (calendar-redraw)
    (appt-activate t)))


;;from http://emacs.rubikitch.com/japanese-holidays/
(with-eval-after-load "calendar"
  (require 'japanese-holidays)
  (setq calendar-holidays 
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq calendar-mark-holidays-flag t)   
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  )

(defun update-schedule()
  """update schedule using caldav calendar
      the default diary file should have a #include \"~/remote-diary\""""
      (interactive)
      (update-calendar-from-url
       (concat "http://localhost:1080/users/" corp-mail "/calendar/")
       "~/remote-diary")
      )

(when (daemonp) (update-schedule))


;; WANDERLUST
;; --------------------------------------
(autoload 'wl "wl" "Wanderlust" t)

(defvar refile-rules-filename "~/.emacs.d/wl-refile-settings.el")
(defun load-refile-rules()
  "sets wl-refile-rule-alist using an external file"
  (interactive)
  (load refile-rules-filename)
  )

(load-file-when-existing refile-rules-filename)

(with-eval-after-load "wl"
  ;;source http://emacs-fu.blogspot.com/2009/09/wanderlust-tips-and-tricks.html
  (define-key wl-summary-mode-map (kbd "b a") 
    '(lambda()(interactive)(wl-summary-refile (wl-summary-message-number) "%archive")))
  (add-to-list 'wl-summary-sort-specs 'reply-date))

;; from https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/apps/wanderlust.el

(defun string-max2 (x y)
  (cond ((string< x y) y)
        ('t x)))

(defun thread-number-get-date (x)
  (timezone-make-date-sortable (elmo-msgdb-overview-entity-get-date
                                (elmo-message-entity
                                 wl-summary-buffer-elmo-folder x))))

(defun thread-get-family (x)
  (cons x (wl-thread-entity-get-descendant (wl-thread-get-entity x))))

(defun max-reply-date (x)
  (cond ((eq 'nil x)
         'nil)
        ((eq 'nil (cdr x))
         (thread-number-get-date (car x)))
        ('t
         (string-max2 (thread-number-get-date (car x))
                      (max-reply-date (cdr x))))))

(defun wl-summary-overview-entity-compare-by-reply-date (a b)
  "Compare message A and B by latest date of replies including thread."
  (string<
   (max-reply-date (thread-get-family (elmo-message-entity-number a)))
   (max-reply-date (thread-get-family (elmo-message-entity-number b)))))


(defun sort-by-rev-date () 
   (wl-summary-rescan "!reply-date"))

(add-hook 'wl-summary-prepared-hook 'sort-by-rev-date)



;; IMAP
(setq elmo-imap4-default-server "localhost"
      elmo-imap4-default-user (concat corp-domain "\\" corp-username)
      elmo-imap4-default-authenticate-type 'login
      elmo-imap4-default-port '1143
      elmo-imap4-default-stream-type 'tls
      elmo-imap4-use-modified-utf7 nil
      )

;; SMTP
(setq wl-smtp-connection-type nil
      wl-smtp-posting-port 1025
      wl-smtp-authenticate-type "login"
      wl-smtp-posting-user (concat corp-domain "\\" corp-username)
      wl-smtp-posting-server "localhost"
      wl-local-domain "localhost"
      wl-message-id-domain "localhost")
;;http://emacs-fu.blogspot.com/2009/06/e-mail-with-wanderlust.html
(setq wl-from corp-mail-from
      wl-trash-folder   "%Trash"
      wl-message-buffer-prefetch-threshold nil
      elmo-message-fetch-confirm nil 
      mime-edit-split-message nil
      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list '("^To:"
                                      "^Cc:"                            
                                      "^From:"
                                      "^Subject:"
                                      "^Date:")
      wl-summary-auto-refile-skip-marks nil ;to auto-refile unread messages
      wl-fcc-force-as-read t               ;; mark sent messages as read
      ;wl-fcc "%Sent"                       ;; sent msgs go to the "sent"-folder

      )

;;to fix =iso-2022-jp... in headers
;;source http://d.hatena.ne.jp/siwazaki/20080102/1199283553

(defun eword-decode-cr (s &optional start-column max-column)
"eword-decode-and-unfold-unstructured-field-body w\ carriage returns"
(let ((l))
  (setq l (split-string (eword-decode-and-unfold-unstructured-field-body s) ","))
  (format "[%d] %s" (length l) (string-join l ",\n"))))

(defun eword-decode-date(time &rest args)
  "date formatter"
  (format-time-string "%a, %d %b %Y %T" (date-to-time time)))

(setq wl-default-draft-cite-date-format-string nil )

(mime-set-field-decoder
 'From nil 'eword-decode-and-unfold-structured-field-body)
(mime-set-field-decoder
 'To 'wide     'eword-decode-cr)
(mime-set-field-decoder
 'Date 'wide 'eword-decode-date)
 
;;source http://d.hatena.ne.jp/sasakyh/touch/20100805/1280989327
;; and https://stackoverflow.com/questions/25109968/in-emacs-how-to-open-file-in-external-program-without-errors
(defvar my-mime-preview-play-current-entity-appname "fiber"
   "program used to open (xdg-open etc)")
(cond
 ((string-match "apple-darwin" system-configuration)
  (setq my-mime-preview-play-current-entity-appname "open")
  )
 ((string-match "linux" system-configuration)
  (setq my-mime-preview-play-current-entity-appname "xdg-open")
  ))
(unless (functionp #'mime-preview-play-current-entity-orig)
  (fset #'mime-preview-play-current-entity-orig
        (symbol-function #'mime-preview-play-current-entity)))
(defun mime-preview-play-current-entity (&optional ignore-examples mode)
  (interactive "P")
  (if (and mode (not (equal mode "play")))
      (mime-preview-play-current-entity-orig ignore-examples mode)
    (let* ((entity (get-text-property (point) 'mime-view-entity))
           (name (mime-entity-safe-filename entity))
           (filename (expand-file-name (if (and name (not (string= name "")))
                                           name
                                         (make-temp-name "EMI"))
                                       (make-temp-file "EMI" 'directory))))
      (mime-write-entity-content entity filename)      
      (let ((process-connection-type nil)) (start-process "" nil my-mime-preview-play-current-entity-appname filename))  
      )))
    

;; SAMBA
;; --------------------------------------


(defun extract-corp-url(url)
  "guess the correct url from malformed ones"
  (if (and url (string-match "[^0-9]+\\([0-9]+\\)+[^0-9]+\\([0-9]+\\)$\\(.+\\)" url))
      (format corp-smb-format (match-string 1 url)
              (match-string 2 url) (match-string 3 (subst-char-in-string ?\\ ?/ url)))
    nil)
  )

(defun open-nautilus-samba()
  "open the link from w3m-anchor (if available) or yank buffer"
  (interactive)
  (let ((url nil))
    (when (fboundp 'w3m-anchor) (setq url (extract-corp-url (w3m-anchor))))    ;first try if it's a mime part
    (unless url                                                                ;if not, search the yank buffer
      (let ((yankurl))
        (setq yankurl (buffer-substring-no-properties (region-beginning) (region-end)))
        (setq url (extract-corp-url yankurl))                                 ;if the yank buffer seems legit ok
        (unless url (setq url (format corp-smb-format "XX" "XX" yankurl) ))));otherwise give an example            
    (setq url (read-from-minibuffer "Filename: " url))                        ;confirm the url
    (setq url (concat "smb://" corp-domain ";" corp-username "@" (subst-char-in-string ?\\ ?/ url)))
    (message "Opening %s" url)
    (let ((process-connection-type nil)) (start-process "" nil "nautilus" url))))
    
(global-set-key "\C-cn" 'open-nautilus-samba)

(defun open-url-with-xdg()
  "open url with xdg-open"
  (interactive)
  (let ((url))
    (setq url (buffer-substring-no-properties (region-beginning) (region-end)))
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" url))))

(global-set-key "\C-cu" 'open-url-with-xdg)
