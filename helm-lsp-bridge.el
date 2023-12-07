
;; TODO 开关 helm-lsp-bridge 功能
(defcustom helm-tramp-to-lsp-bridge t
  "是否自动将 helm tramp 路径转化为 lsp bridge remote url"
  :type 'boolean)

;; mask helm-find-file-or-marked, 将 tramp 路径跳转至 lsp-bridge
(defun helm-find-file-or-marked (candidate)
  "Open file CANDIDATE or open helm marked files in separate windows.
Called with one prefix arg open files in separate windows in a
vertical split.
Called with two prefix arg open files in background without
selecting them."
  (let ((marked (helm-marked-candidates :with-wildcard t))
        (url-p (and helm--url-regexp ; we should have only one candidate.
                    (string-match helm--url-regexp candidate)))
        (ffap-newfile-prompt helm-ff-newfile-prompt-p)
        (find-file-wildcards nil)
        (helm--reading-passwd-or-string t))
    (if (cdr marked)
        (if (equal helm-current-prefix-arg '(16))
            (mapc 'find-file-noselect marked)
          ;; If helm-current-prefix-arg is detected split is done
          ;; vertically.
          (helm-window-show-buffers (mapcar 'find-file-noselect marked)))
      (let ((dir (and (not url-p) (helm-basedir candidate))))
        (cond ((and dir (file-directory-p dir))
               (find-file (substitute-in-file-name candidate)))
              (url-p (find-file-at-point candidate))
              ;; A a non--existing filename ending with /
              ;; Create a directory and jump to it.
              ((and (not (file-exists-p candidate))
                    (string-match "/$" candidate))
               (helm-ff--mkdir candidate 'helm-ff))
              ;; A non--existing filename NOT ending with / or
              ;; an existing filename, create or jump to it.
              ;; If the basedir of candidate doesn't exists,
              ;; ask for creating it.
              (dir
               (helm-ff--mkdir dir)
               (find-file candidate))
              ;; Find file at `default-directory' when basedir is
              ;; unspecified e.g user hit C-k foo RET.
              (t (find-file candidate)))))))

;; 判断 helm candidate 是否为 tramp 路径
(defun helm-lb--is-candidate-tramp (candidate)
  ""
  (let* ((parts (split-string candidate ":"))
	(prot (car parts))
	(conn (cadr parts))
	(path (car (last parts)))
	(user-conn (split-string conn "@"))
	(host-port (split-string (if (>= (length user-conn) 2) (cadr user-conn) (car user-conn)) "#"))
	(user (if (>= (length user-conn) 2) (car user-conn)))
	(host (if (>= (length host-port) 2) (car host-port) (car host-port)))
	(port (if (>= (length host-port) 2) (cadr host-port)))
	)
    (if (string-match "\\(/ssh\\|/sss\\)" prot)
	(if user
	    (if port
		(concat user "@" host ":" port ":" path)
	      (concat user "@" host ":" path))
	  (if port
	      (concat host ":" port ":" path)
	    (concat host ":" path))))
  ))

;; 使用 lsp-bridge 打开 tramp 路径
(defun helm-lb--open-lsb-bridge-remote (candidate)
  ""
  )

;; mask helm-find-files, 将 lsp-bridge remote file 转换为 tramp 路径

;; 判断 buffer 是否为 lsp-bridge remote file

;; 将 lsp remote file buffer 转换为 tramp 路径
