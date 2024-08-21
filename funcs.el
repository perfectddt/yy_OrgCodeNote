;; https://www.spacemacs.org/doc/LAYERS.html#funcsel
;; 还是可以添加 when package-used
(defun yy-add-star-to-line-number-display (line-number)
  "在指定行号的行前添加星号以高亮显示，先删除原有覆盖层。"
  (save-excursion
    (goto-line line-number)
    (let ((beginning (line-beginning-position)))
      ;; 删除现有的覆盖层
      (dolist (overlay (overlays-in beginning (1+ beginning)))
        (delete-overlay overlay))
      ;; 添加新的覆盖层
      (let ((overlay (make-overlay beginning beginning)))
        (overlay-put overlay 'before-string "★ ")
        (overlay-put overlay 'face '(:foreground "red")))))
  (message "yy-add-star-to-line-number-display")
  )

(defun yy-read-sqlite-db-via-command (db-file query)
  "Reads data from an SQLite database file using the sqlite3 command line tool."
  (with-temp-buffer
    (let ((result (call-process "sqlite3" nil t nil db-file query)))
      (message "sqlite3 process result: %s" result)  ; Print the result of the process call
      (message "sqlite3 output:\n%s" (buffer-string))  ; Print the output of sqlite3
      (if (eq result 0)
          (split-string (buffer-string) "\n" t)
        (message "sqlite3 command failed with result: %s" result)
        nil)))
  )

(defun modify-filename-for-table (filename)
  "将文件名转换为适用于数据库表名的格式。"
  (let ((modified-filename (replace-regexp-in-string "\\$" "" filename)))
    (dolist (char '(" " "." "+" "-" "/" "\\\\" ":"))
      (setq modified-filename (replace-regexp-in-string (regexp-quote char) "_" modified-filename)))
    (concat modified-filename "_table"))
  )


(defun yy-highlight-lines-in-db ()
  "高亮显示数据库中指定的行号。"
  (interactive)
  (let* ((current-file (buffer-file-name))
         (db-file "yy_orgcodenote_my_database.db")
         (current-file-name (file-name-nondirectory current-file))
         (table-name (modify-filename-for-table current-file-name)))
    (when (file-exists-p db-file)
      (let ((line-numbers (yy-read-sqlite-db-via-command db-file (format "SELECT code_num FROM %s;" table-name))))
        (dolist (line-number line-numbers)
          (yy-add-star-to-line-number-display (string-to-number line-number))))))
  )

;; org创建数据库
(defun yy-python-orgfind ()
  "Run Python script OrgFind.py with current file's absolute path and database."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-file-name (file-name-nondirectory current-file))
         (current-dir (file-name-directory current-file))
         (python-script (concat (expand-file-name my-default-directory) "/.spacemacs.d/layer/yy_OrgCodeNote/OrgFind.py"))
         (database "yy_orgcodenote_my_database.db")
         (processed-table (modify-filename-for-table current-file-name))
         )  
    (start-process "orgfind-process" nil "python"
                   (format "%s" python-script)
                   "--file"
                   (format "%s" current-file)
                   "--database"
                   (format "%s" database)
                   "-t"
                   (format "%s" processed-table)))
  (message "yy-python-orgfind")
  )
;; org拆分库里的表，并移动表
(defun yy-python-splittable ()
  "Run Python script OrgFind.py with current file's absolute path and database."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-file-name (file-name-nondirectory current-file))
         (current-dir (file-name-directory current-file))
         (python-script (concat (expand-file-name my-default-directory) "/.spacemacs.d/layer/yy_OrgCodeNote/SplitTable.py"))
         (database "yy_orgcodenote_my_database.db")
         (processed-table (modify-filename-for-table current-file-name))
         )  
    (start-process "splittable-process" nil "python"
                   (format "%s" python-script)
                   "--database"
                   (format "%s" database)
                   "-t"
                   (format "%s" processed-table)))
  (message "yy-python-splittable")
  )

(defun yy-open-file-from-current-line ()
  "Prints and opens the file path from the current line, removing spaces from the path."
  (interactive)
  (let* ((current-line (thing-at-point 'line))
         (file-path (when (string-match ":LOCATION: \\(.*\\)" current-line)
                      (match-string 1 current-line)))
         (absolute-file-path (when file-path
                               (expand-file-name file-path (file-name-directory (buffer-file-name)))))
         (absolute-file-path-no-spaces (when absolute-file-path
                                         (replace-regexp-in-string " " "" absolute-file-path))))
    (when absolute-file-path-no-spaces
      (message "File path: %s" absolute-file-path-no-spaces)
      (find-file absolute-file-path-no-spaces)
      ))
  (message "yy-open-file-from-current")
  )

;; 在数据库里查找代码文件
(defun yy-python-searchcodeplus ()
  "Run Python script OrgFind.py with current file's absolute path and database."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-file-name (file-name-nondirectory current-file))
         (current-dir (file-name-directory current-file))
         (python-script (concat (expand-file-name my-default-directory) "/.spacemacs.d/layer/yy_OrgCodeNote/SearchCodePlus.py"))
         (database "yy_orgcodenote_my_database.db")
         (processed-table (modify-filename-for-table current-file-name))
         )  
    (start-process "searchcodeplus-process" nil "python"
                   (format "%s" python-script)
                   "--database"
                   (format "%s" database)
                   "--table"
                   (format "%s" processed-table)))
  (message "yy-python-searchcodeplus")
  )
(defun yy-jump-to-line-in-db ()
  "Jump to the line in the database specified by the current line number."
  (interactive)
  (let* ((current-line-number (line-number-at-pos))
         (db-file "yy_orgcodenote_my_database.db")
         (current-file (buffer-file-name))
         (current-file-name (and current-file (file-name-nondirectory current-file)))
         (table-name (and current-file-name (modify-filename-for-table current-file-name)))
         (query (and table-name (format "SELECT org_location, org_num FROM %s WHERE code_num = %d;" table-name current-line-number)))
         (result (and query (yy-read-sqlite-db-via-command db-file query))))

    ;; (message "current-line-number: %d" current-line-number)
    ;; (message "db-file: %s" db-file)
    ;; (message "current-file: %s" current-file)
    ;; (message "current-file-name: %s" current-file-name)
    ;; (message "table-name: %s" table-name)
    ;; (message "query: %s" query)

    (if (not result)
        (message "跳转失败")
      (let ((output (car result)))
        (when (string-match "\\(.*\\)|\\([0-9]+\\)" output)
          (let ((org-file (match-string 1 output))
                (target-line-number (string-to-number (match-string 2 output))))
            ;; (message "org-file: %s" org-file)
            ;; (message "target-line-number: %d" target-line-number)
            (find-file-other-window org-file)
            (goto-line target-line-number)
            (message "跳转到行号: %d" target-line-number)))))))

(defun yy-python-orgfind-and-splittable ()
  "首先执行yy-python-orgfind，然后执行yy-python-splittable。"
  (interactive)
  (yy-python-orgfind)  ; 调用第一个函数
  (yy-python-splittable)  ; 调用第二个函数
)
(defun yy-python-searchcodeplus-and-highlight ()
  "首先执行yy-python-searchcodeplus，然后执行yy-highlight-lines-in-db。"
  (interactive)
  (yy-python-searchcodeplus)  ; 调用第一个函数
  (yy-highlight-lines-in-db)  ; 调用第二个函数
  )

(defun process-and-copy-filename ()
  "处理带路径的文件名并复制到剪贴板。
删除路径中从开始到'BaiduSyncdisk'的部分，然后使用`modify-filename-for-table`处理文件名。"
  (interactive)
  (let* ((full-path (buffer-file-name))
         (path-after-baiduSyncdisk (when full-path
                                     (save-match-data
                                       (string-match "BaiduSyncdisk" full-path))
                                     (substring full-path (match-beginning 0))))
         (modified-filename (when path-after-baiduSyncdisk
                              (modify-filename-for-table path-after-baiduSyncdisk))))
    (when modified-filename
      (kill-new modified-filename)
      (message "处理后的文件名: %s" modified-filename))))

(defun yy-process-and-copy-filename-code-to-org ()
  "处理并复制当前文件的路径到剪贴板。"
  (interactive)
  (let* ((current-file-path (buffer-file-name))
         (processed-path (replace-regexp-in-string "^.*BaiduSyncdisk/" "" current-file-path))
         (table-name (modify-filename-for-table processed-path))
         (code-name (replace-regexp-in-string "_table$" "_code" table-name)))
    (kill-new code-name)
    (message "处理后的文件名: %s" code-name))
  )

(defun yy-org-rename-16 ()
  "Rename current Org mode file by removing the first 16 characters from its name."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (new-name (if current-file
                       (concat (substring (file-name-nondirectory current-file) 16))
                     nil)))
    (if (and new-name (file-exists-p current-file))
        (progn
          (rename-file current-file new-name 1)
          (kill-buffer (current-buffer))
          (find-file new-name))
      (message "Error: Unable to rename the file."))))

(defun yy-rename-file ()
  "Prompt with the current file name for renaming."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
        (let ((new-name (read-string "New name: " (file-name-nondirectory current-file))))
          (if (and new-name (not (string-equal new-name "")))
              (progn
                (let ((new-file-path (concat (file-name-directory current-file) new-name)))
                  (rename-file current-file new-file-path)
                  (kill-buffer (current-buffer))
                  (find-file new-file-path)
                  (message "File renamed to %s" new-file-path)))
            (message "No new name provided, no action taken.")))
      (message "This buffer is not visiting a file!"))))

(defvar yy-posframe-name "code-posframe")

(defun yy-show-posframe-at-point (file-path line-number)
  "Display a posframe with text near point, showing contents from FILE-PATH starting at LINE-NUMBER."
  (interactive "fFile path: \nnLine number: ")
  (let ((file-contents (yy-display-file-line file-path line-number)))
    (if file-contents
        (posframe-show yy-posframe-name
                       :string file-contents
                       :position (point)
                       :background-color "#333333"
                       :foreground-color "#ffffff")
      (message "No content or file not readable"))))

(defun yy-hide-posframe ()
  "Hide the posframe."
  (interactive)
  (posframe-hide yy-posframe-name)
)

(defun yy-display-file-line (file-path line-number)
  "从指定的 LINE-NUMBER+1 行开始，在 FILE-PATH 文件中寻找以'^+'开头的行，返回从 LINE-NUMBER 行到找到的行前一行的内容。"
  (if (and file-path line-number)
      (if (file-readable-p file-path)
          (with-temp-buffer
            (insert-file-contents file-path)
            (goto-char (point-min))
            (forward-line line-number)
            (let ((end-line nil))
              (while (and (not end-line) (not (eobp)))
                (forward-line 1)
                (when (looking-at "^\\*+ ")
                  (setq end-line (1- (line-number-at-pos)))))
              (unless end-line
                (setq end-line (line-number-at-pos (point-max))))
              (goto-char (point-min))
              (forward-line (1- line-number))
              (buffer-substring-no-properties (point)
                                              (progn
                                                (goto-char (point-min))
                                                (forward-line end-line)
                                                (point))))))))

(defun yy-read-to-line-in-db ()
  "Jump to the line in the database specified by the current line number."
  (interactive)
  (let* ((current-line-number (line-number-at-pos))
         (db-file "yy_orgcodenote_my_database.db")
         (current-file (buffer-file-name))
         (current-file-name (and current-file (file-name-nondirectory current-file)))
         (table-name (and current-file-name (modify-filename-for-table current-file-name)))
         (query (and table-name (format "SELECT org_location, org_num FROM %s WHERE code_num = %d;" table-name current-line-number)))
         (result (and query (yy-read-sqlite-db-via-command db-file query)))
         (org-path "")
         (org-number-line ""))
    (when result
      (setq result-string (car result))  ; Assuming the result is a single string
      (if (string-match "\\(.*\\)|\\([0-9]+\\)" result-string)
          (progn
            (setq org-path (match-string 1 result-string))
            (setq org-number-line (string-to-number (match-string 2 result-string)))  ; Convert string to number
            ;; Uncomment for debugging:
            (message "org-path: %s, org-number-line: %s" org-path org-number-line)
            ;; (yy-display-file-line org-path org-number-line)
            (yy-show-posframe-at-point org-path org-number-line)
            )
        (message "无法解析数据库输出: %s" result-string)))
    (unless result
        (message "跳转失败"))))

(defun yy-find-starred-lines ()
  "查找当前 buffer 中由覆盖层设置的含有星号★的行，并显示在新 buffer 中。"
  (interactive)
  (let ((starred-overlays '()))
    (save-excursion
      (goto-char (point-min))
      (mapc (lambda (overlay)
              (when (and (overlay-get overlay 'before-string)
                         (string-match "★" (overlay-get overlay 'before-string)))
                (push (list (line-number-at-pos (overlay-start overlay))
                            (current-buffer)
                            (overlay-start overlay))
                      starred-overlays)))
            (overlays-in (point-min) (point-max))))

    (if starred-overlays
        (let ((new-buffer (generate-new-buffer "*Starred Lines*"))
              (original-buffer (current-buffer))) ;; 记录当前 buffer 作为原始 buffer
          (with-current-buffer new-buffer
            (dolist (overlay-info starred-overlays)
              (let ((line-number (car overlay-info))
                    (match-begin (caddr overlay-info)))
                (insert (format "Line %d\n" line-number))
                (insert-text-button
                 "Jump to line"
                 'action `(lambda (_) ;; 使用 `(_) 来忽略参数
                            (interactive)
                            (switch-to-buffer-other-window ,original-buffer)
                            (goto-char ,match-begin))
                 'follow-link t)
                (insert "\n"))))
          (display-buffer new-buffer
                          `(display-buffer-pop-up-window
                            (inhibit-same-window . t)))) ;; 确保在新窗口中打开
      (message "No starred lines found in current buffer."))))
