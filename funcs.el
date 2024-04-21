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

    (if (not result)
        (message "跳转失败")
      (let ((org-file (car result))
            (target-line-number (string-to-number (cadr result))))
        (when (and org-file target-line-number)
          (find-file org-file)
          (goto-line target-line-number)
          (message "跳转到行号: %d" target-line-number)))))
  (message "yy-jump-to-line-in-db")
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
            (message "org-file: %s" org-file)
            (message "target-line-number: %d" target-line-number)
            (find-file org-file)
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
