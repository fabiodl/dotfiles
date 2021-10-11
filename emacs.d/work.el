(defvar corp-packages
  '(w3m
   ;wanderlust
    notmuch
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

;; MAILS
;; --

(defvar refile-rules-filename "~/.emacs.d/wl-refile-settings.el")
(defun load-refile-rules()
  "sets wl-refile-rule-alist using an external file"
  (interactive)
  (load refile-rules-filename)
  )

(load-file-when-existing refile-rules-filename) ;sets wl-refile-rule-alist
(load-file-when-existing "~/.emacs.d/notmuch-settings.el")
(require 'notmuch nil 'noerror) ;this makes notmuch optional

(defun my-confirm-sending ()
  "Allow user to quit when current message subject is empty."
  (let ((ccs (message-field-value "Cc")))
    (or (yes-or-no-p (concat "Send mail to "
                             (message-field-value "To")
                             (unless (null ccs) (concat " cc:" ccs))
                             "?"))
      (keyboard-quit))))


(add-hook 'notmuch-mua-send-hook #'my-confirm-sending)

;; SAMBA
;; --------------------------------------


(defun extract-corp-url(url)
  "guess the correct url from malformed ones"
  (if (and url (string-match "[^0-9]+\\([0-9]+\\)+[^0-9]+\\([0-9]+\\)$\\(.+\\)" url))
      (format corp-smb-format (match-string 1 url)
              (match-string 2 url) (match-string 3 (subst-char-in-string ?\\ ?/ url)))
    nil)
  )

(defun open-nautilus-samba(extract)
  "open the link from w3m-anchor (if available) or yank buffer"
  (interactive)
  (let ((url nil))
    (when (fboundp 'w3m-anchor) (setq url (extract-corp-url (w3m-anchor))))    ;first try if it's a mime part
    (unless url                                                                ;if not, search the yank buffer
      (let ((yankurl))
        (setq yankurl (replace-regexp-in-string "\n" "" (buffer-substring-no-properties (region-beginning) (region-end))))        
        (setq url (if extract (extract-corp-url yankurl) yankurl))           ;if the yank buffer seems legit ok
        (unless url (setq url (format corp-smb-format "XX" "XX" yankurl) ))));otherwise give an example            
    (setq url (read-from-minibuffer "Filename: " url))                        ;confirm the url
    (setq url (concat "smb://" corp-domain ";" corp-username "@" (subst-char-in-string ?\\ ?/ url)))
    (message "Opening %s" url)
    (let ((process-connection-type nil)) (start-process "" nil "nautilus" url))))
    
(global-set-key "\C-cn" (lambda () (interactive) (open-nautilus-samba nil)))
(global-set-key "\C-cN" (lambda () (interactive) (open-nautilus-samba t)))

(defun open-url-with-xdg()
  "open url with xdg-open"
  (interactive)
  (let ((url))
    (setq url (buffer-substring-no-properties (region-beginning) (region-end)))
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" url))))

(global-set-key "\C-cu" 'open-url-with-xdg)

(defun buffer-as-jis ()
  ""
  (interactive)
  (read-only-mode -1)
  (decode-coding-region (point-min) (point-max) 'iso-2022-jp)
  (read-only-mode +1)
  )
(global-set-key "\C-cj" 'buffer-as-jis)
(setq mail-host-address "mail-host-address.com")
