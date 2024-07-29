(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar binary-fetcher
  (let ((os-uname (cons system-type system-configuration))
        (homedir (expand-file-name "~")))
    (list :binary-path nil
          :binary-url nil
          :os-uname os-uname
          :homedir homedir)))

(defun generate-temp-path (n)
  (let ((charset "abcdefghijklmnopqrstuvwxyz0123456789")
        (random-string ""))
    (dotimes (_ n)
      (let ((random-index (random (length charset))))
        (setq random-string (concat random-string (substring charset random-index (1+ random-index))))))
    (concat random-string ".tmp")))

(defun binary-fetcher-platform ()
  (pcase system-type
    ('darwin "macosx")
    ('gnu/linux "linux")
    ('windows-nt "windows")
    (_ "")))

(defun supermaven-fetch-binary ()
  (interactive)
  (let ((local-binary-path (binary-fetcher-local-binary-path)))
    (if (file-exists-p local-binary-path)
            (progn
                (message (format "Supermaven binary already downloaded to %s" local-binary-path))
                local-binary-path)
      (make-directory (binary-fetcher-local-binary-parent-path) t)
      (let ((url (binary-fetcher-discover-binary-url)))
        (if (null url)
            nil
          (message "Downloading Supermaven binary, please wait...")
          (let ((temp-path (generate-temp-path 10))
                (platform (binary-fetcher-platform))
                (output-buffer (get-buffer-create "*supermaven-download-output*"))
                (error-buffer (get-buffer-create "*supermaven-download-error*")))
            (with-current-buffer error-buffer
                (erase-buffer))
            (if (string= platform "windows")
                (shell-command
                 (format "powershell -Command Invoke-WebRequest -Uri '%s' -OutFile '%s'"
                         url local-binary-path))
              (shell-command (format "curl -sS -o %s %s" temp-path url) output-buffer error-buffer))
            (if (= (buffer-size error-buffer) 0)
                (progn
                  (unless (string= platform "windows")
                    (rename-file temp-path local-binary-path t))
                  (message "Downloaded binary sm-agent to %s" local-binary-path)
                  (set-file-modes local-binary-path #o755)
                  local-binary-path)
              (message "sm-agent download failed")
              nil)))))))

(defun binary-fetcher-get-arch ()
  (if (string-match-p "arm\\|aarch64" system-configuration)
      "aarch64"
    (if (string-match-p "x86_64" system-configuration)
        "x86_64"
      "")))

(defun binary-fetcher-discover-binary-url ()
  (let* ((platform (binary-fetcher-platform))
         (arch (binary-fetcher-get-arch))
         ;;(url (format "https://supermaven.com/api/download-path?platform=%s&arch=%s&editor=neovim"
         ;;             platform arch))
         (url "https://supermaven.com/api/download-path?platform=linux&arch=x86_64&editor=neovim")
         response)
    (setq response
          (if (string= platform "windows")
              (shell-command-to-string
               (format "powershell -Command Invoke-WebRequest -Uri '%s' -UseBasicParsing | Select-Object -ExpandProperty Content"
                       url))
            (shell-command-to-string (format "curl -s \"%s\"" url))))
    (setq response (string-trim response))
    (condition-case nil
        (let ((json-object-type 'plist)
              (json-array-type 'list))
          (plist-get (json-read-from-string response) :downloadUrl))
      (error (message "Unable to find download URL for Supermaven binary")
             nil))))

(defun binary-fetcher-local-binary-path ()
  (let ((parent-path (binary-fetcher-local-binary-parent-path)))
    (expand-file-name
     (if (string= (binary-fetcher-platform) "windows")
         "sm-agent.exe"
       "sm-agent")
     parent-path)))

(defun binary-fetcher-local-binary-parent-path ()
        (expand-file-name
                (format ".supermaven/binary/v15/%s-%s"
                        (binary-fetcher-platform)
                        (binary-fetcher-get-arch))
                home-dir))

(defconst supermaven-buffer "*supermaven-messages*")
;; home directory
(defvar home-dir gnus-home-directory)
;; sm-agent should be in ~/.supermaven/bin/
(defcustom supermaven-blob-path (binary-fetcher-local-binary-path)
  "Path to the supermaven blob"
  :group 'supermaven)


(defvar supermaven-process nil)
(defvar supermaven-compl-callback nil)
(defvar supermaven-current-state-id -1)
(defvar supermaven-current-compl "")

(defun supermaven-filter(process string)
  (let ((buf(get-buffer-create "*supermaven*")))
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-max))
	  (insert string)))

  (while (string-match "SM-MESSAGE" string)
	(let ((offset (string-match "SM-MESSAGE" string))
		  (next (string-match "SM-MESSAGE" string 10)))
	  (when offset
		(supermaven-process (json-parse-string (substring string 11 (if next next (length string))))))
	  (if next
		  (setq string (substring string next))
		(setq string "")))))

(defun supermaven-process-start(compl-callback)
  (when supermaven-process
	(delete-process supermaven-process))
  (setq supermaven-compl-callback compl-callback)
  (setq supermaven-process (make-process :name "supermaven"
										 :command (list supermaven-blob-path "stdio")
										 :connection-type 'pipe
										 :filter 'supermaven-filter)))

(defun supermaven-greetings()
  (let ((json (json-serialize #s(hash-table test equal data ("kind" "greeting")))))
	(process-send-string supermaven-process (concat json "\n")))
  )

(defun supermaven-process(json)
  (cond ((string= (gethash "kind" json) "response")
		 (supermaven-update-state-id json))
		((string= (gethash "kind" json) "metadata")
		 (supermaven-update-metadata json))
		((string= (gethash "kind" json) "activation_request")
		 (save-excursion
		   (set-buffer (get-buffer-create supermaven-buffer))
		   (goto-char (point-max))
		   (insert (concat "Activation request : "
						   (gethash "activateUrl" json) "\n"))))
		((string= (gethash "kind" json) "activation_success")
		 (save-excursion
		   (set-buffer (get-buffer-create supermaven-buffer))
		   (goto-char (point-max))
		   (insert "Supermaven was activated successfully.\n")))
		))

(defun supermaven-send(message)
	(process-send-string supermaven-process (concat message "\n"))
)

(defun supermaven-update-state-id(json)
  (let ((state-id (string-to-number (gethash "stateId" json))))
	(if (= state-id supermaven-current-state-id)
		(let ((items (gethash "items" json)))
		  (seq-doseq (item items)
			(cond ((string= (gethash "kind" item) "text")
				   (setq supermaven-current-compl (concat supermaven-current-compl (gethash "text" item))))
				  ((string= (gethash "kind" item) "end")
				   (when supermaven-compl-callback
					 (supermaven-compl-callback supermaven-current-compl)
					 (setq supermaven-current-compl ""
						   supermaven-current-state-id (1+ supermaven-current-state-id)))))))
	  (progn
		(setq supermaven-current-compl ""
			  supermaven-current-state-id state-id)
		(supermaven-update-state-id json)))))


(defun supermaven-update-metadata(json))

(provide 'supermaven-process)
