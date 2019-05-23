(setq notmuch-search-oldest-first nil
      notmuch-fcc-dirs "Sent")

(defun flatten (LIST)
  (if LIST
      (append (car LIST) (flatten (cdr LIST)))
    nil))

(defvar notmuch-leave-in-inbox '())

(defun wl-convert-to-notmuch-tags()
  ""
  (let ( (tagassoc '(("From" . "from") ("To" . "to") ("Subject" . "subject")))
         (removedtags "-inbox ")
         )
    (flatten (remove nil (mapcar  (lambda (x)
                                    (let ( (nmtag (assoc (car x) tagassoc)) )
                                      (if nmtag (mapcar (lambda(y) (concatenate 'string
                                                                                (if (member (substring (cdr y) 1) notmuch-leave-in-inbox) "" removedtags)
                                                                                (s-replace "%" "+" (cdr y)) " " (cdr nmtag) ":\"/" (car y) "/\"") )
                                                        (cdr x) ))
                                      ))
                                  wl-refile-rule-alist)))))


(defun get-notmuch-tag-rules()
  ""
  (concat (string-join  (wl-convert-to-notmuch-tags) "\n" )
                        "\n"
                        (read-file-contents "~/.emacs.d/.notmuchextratags")))


(defun read-file-contents (fname)
  ""
  (when (file-exists-p fname)
    (with-temp-buffer
      (insert-file-contents fname)
      (buffer-string))))


(defun get-notmuch-tags ()
  ""
  (mapcar (lambda(x) (substring x 1)) (delete-dups (seq-filter (lambda (x) (string= (substring x 0 1) "+"))   (split-string (get-notmuch-tag-rules))))))


(defun get-afew-mailmover-rules ()
  ""
  (let* ((tags (get-notmuch-tags))
         (inboxtag "inbox"))
    (concat "[MailMover]\n"
            "folders = INBOX \n"
            "\n"
            "#rules\n"
            "INBOX = "
            (string-join (mapcar (lambda(x) (concat "'tag:" x " AND NOT tag:" inboxtag "':" x " ")) tags)) " "
            "'" (string-join (mapcar (lambda(x) (concat "NOT tag:" x)) tags) " AND ") " AND NOT tag:" inboxtag
            "':archive\n")))

(defun export-notmuch-tag-rules()
  ""
  (interactive)
  (write-region (get-notmuch-tag-rules) nil "~/.notmuchtags"))


(defun export-afew-mailmover-rules()
  ""
  (interactive)
  (write-region (get-afew-mailmover-rules) nil "~/.config/afew/config"))


;from https://kkatsuyuki.github.io/notmuch-conf/
(defun notmuch-sync ()
    "execute offlineimap"
    (interactive)
    (set-process-sentinel
     (start-process-shell-command "offlineimap"
                                  "*offlineimap*"
                                  "offlineimap -o")
     '(lambda (process event)
        (notmuch-refresh-all-buffers)
        (let ((w (get-buffer-window "*offlineimap*")))
          (when w
            (with-selected-window w (recenter (window-end)))))))
    (display-buffer "*offlineimap*"))


