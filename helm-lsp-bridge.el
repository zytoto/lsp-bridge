
;; 开关 helm-lsp-bridge 功能
(defcustom helm-tramp-to-lsp-bridge t
  "是否自动将 helm tramp 路径转化为 lsp bridge remote url"
  :type 'boolean)

(defun strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt)
      txt)

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
        (cond ((and helm-tramp-to-lsp-bridge (string-prefix-p "/ssh:" candidate))   ; 如果打开的是 tramp ssh
	       (lsp-bridge-call-async   ; 使用 lsp bridge 打开文件
		"open_remote_file"
		(strip-text-properties
		 (replace-regexp-in-string
		  "#" ":" 
		  (substring candidate 5)))
		(list :line 0 :character 0)))
	      ((and dir (file-directory-p dir))
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

;; mask helm-find-files, 将 lsp-bridge remote file 转换为 tramp 路径
(defun helm-find-files (arg)
  "Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on
files."
  (interactive "P")
  (let* (tramp-archive-enabled ; Disable tramp-archive which is
                               ; kicking in unexpectedly.
         (hist            (and arg helm-ff-history (helm-find-files-history nil)))
         (smart-input     (or hist (helm-find-files-initial-input)))
         (default-input   (expand-file-name (helm-current-directory)))
         (input           (cond (lsp-bridge-remote-file-flag   ; 判断 buffer 是否为 lsp-bridge remote file
				 (concat  ; 将 lsp remote file buffer 转换为 tramp 路径
				  "/ssh:"
				  lsp-bridge-remote-file-ssh-user "@" lsp-bridge-remote-file-host
				  "#" lsp-bridge-remote-file-ssh-port ":"
				  (file-name-as-directory
				   (expand-file-name (file-name-directory lsp-bridge-remote-file-path)))))
				((and (null hist)
                                      helm-find-files-ignore-thing-at-point)
                                 default-input)
                                ((and (eq major-mode 'org-agenda-mode)
                                      org-directory
                                      (not smart-input))
                                 (file-name-as-directory
                                  (expand-file-name org-directory)))
                                ((and (eq major-mode 'dired-mode) smart-input)
                                 (file-name-directory smart-input))
                                ((and (not (string= smart-input ""))
                                      smart-input))
                                (t default-input)))
         (input-as-presel (null (file-directory-p input)))
         (presel          (helm-aif (or hist
                                        (and input-as-presel input)
                                        (buffer-file-name (current-buffer))
                                        (and (eq major-mode 'dired-mode)
                                             smart-input))
                              (if (and helm-ff-transformer-show-only-basename
                                       (null hist)
                                       (not (string-match-p "[.]\\{1,2\\}\\'" it)))
                                  (helm-basename it)
                                it))))
    ;; Continue using the same display function as history which used
    ;; probably itself the same display function as inner HFF call,
    ;; i.e. if history was using frame use a frame otherwise use a window.
    (when (and hist (buffer-live-p (get-buffer helm-ff-history-buffer-name)))
      (helm-set-local-variable 'helm-display-function
                               (with-current-buffer helm-ff-history-buffer-name
                                 helm-display-function)
                               'helm--last-frame-parameters
                               (with-current-buffer helm-ff-history-buffer-name
                                 helm--last-frame-parameters)))
    (set-text-properties 0 (length input) nil input)
    (setq current-prefix-arg nil)
    ;; Allow next helm session to reuse helm--last-frame-parameters as
    ;; resume would do.
    (let ((helm--executing-helm-action (not (null hist))))
      (helm-find-files-1 input (and presel (null helm-ff-no-preselect)
                                    (format helm-ff-last-expanded-candidate-regexp
                                            (regexp-quote presel)))))))

(provide 'helm-lsp-bridge)
