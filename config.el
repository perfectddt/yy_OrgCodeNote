;; https://www.spacemacs.org/doc/LAYERS.html#configel

;; (with-eval-after-load 'org-roam
;;   (setq org-roam-dailies-directory "daily/")
;;   (setq org-roam-db-gc-threshold most-positive-fixnum);; 提高性能
;;   (require 'org-roam-dailies);; 启用日记功能
;; (defun get-current-file-path ()
;;   "获取当前缓冲区文件的路径。"
;;   (interactive)
;;   (let ((file-name (buffer-file-name)))
;;     (when file-name
;;       (message "当前文件的路径是: %s" (expand-file-name file-name)))))
;; (spacemacs/copy-directory-path)
;; (spacemacs/copy-file-name)
;; (spacemacs/copy-file-path)


;;   ;; Markdown模板
;;   (add-to-list 'org-roam-capture-templates
;;                '("m" "Markdown" plain "" :target
;;                  (file+head "%<%Y-%m-%dT%H%M%S>_${title}.md" "---\ntitle: ${title}\nid: %<%Y-%m-%dT%H%M%S>\ncategory:#º0_note \n---\n")
;;                  :unnarrowed t))

;;   ;; Zotero 文献模板
;;   (setq my/ref-template
;;         (concat "#+FILETAGS: :º0_research: \n"
;;                 "- tags :: %^{keywords} \n"
;;                 "* %^{title}\n"
;;                 ":PROPERTIES:\n"
;;                 ":Custom_ID: %^{citekey}\n"
;;                 ":URL: %^{url}\n"
;;                 ":AUTHOR: %^{author-or-editor}\n"
;;                 ":NOTER_DOCUMENT: ../../../../zotero-pdf/%^{citekey}.pdf\n"
;;                 ":NOTER_PAGE:\n"
;;                 ":END:"))
;;   (add-to-list 'org-roam-capture-templates
;;                `("r" "Zotero 文献模板" plain
;;                  ,my/ref-template
;;                  :target
;;                  (file+head "ref/${citekey}.org" "#+title: ${title}\n")))

;;   ;; 常用笔记模板
;;   (setq my/note-template
;;         (concat "#+FILETAGS: :º0_note:\n"
;;                 "#+STARTUP: indent\n"
;;                 "- tags :: \n"
;;                 "* %^{title}\n"
;;                 ":PROPERTIES:\n"
;;                 ":END:"))
;;   (add-to-list 'org-roam-capture-templates
;;                `("n" "常用笔记模板" plain
;;                  ,my/note-template
;;                  :target
;;                  (file+head "$%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n")
;;                  :immediate-finish t
;;                  :unnarrowed t))

;;   ;; org-roam-ui
;;   (setq org-roam-ui-sync-theme t;; 同步 Emacs 主题
;;         org-roam-ui-follow t;; 笔记节点跟随
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t)
;;   (setq org-roam-file-extensions '("org" "md"))
;;   (add-to-list 'org-file-apps '("\\.md\\'" . emacs))

;;   ;; mdnote
;;   (require 'md-roam)
;;   (md-roam-mode 1); md-roam-mode must be active before org-roam-db-sync
;;   (setq md-roam-file-extension "md"); default "md". Specify an extension such as "markdown"
;;   ;; 同步库
;;   (org-roam-db-autosync-mode 1); autosync-mode triggers db-sync. md-roam-mode must be already active

;;   )






;; ;; org-roam
;; ;; org-helm
;; (cond
;;  ((eq system-type 'windows-nt)
;;   ;; 默认笔记目录, 提前手动创建好
;;   ;; 给 Windows 设置好环境变量
;;   (setq org-roam-directory (concat (getenv "yynote") "/foam-template-master/masscode/org-mode/"))
;;   ;; Zotero 用 Better BibTeX 导出的 .bib 文件. 可以是多个文件
;;   (setq zot_bib (concat (getenv "yynote") "/zotero-pdf/my-library.bib"))
;;   (setq zot_pdf (concat (getenv "yynote") "/zotero-pdf/")); Zotero 的 ZotFile 同步文件夹
;;   ;; 自定义的 org-roam 文献笔记目录. 我的 org-roam 根目录是 ~/repos/notes
;;   (setq org_refs (concat (getenv "yynote") "/foam-template-master/masscode/org-mode/ref/")))

;;  ((eq system-type 'gnu/linux)
;;   ;; 给 linux 设置好 yynote 环境变量
;;   (setq org-roam-directory (concat (getenv "yynote") "/foam-template-master/masscode/org-mode/"))
;;   (setq zot_bib (concat (getenv "yynote") "/zotero-pdf/my-library.bib"))
;;   (setq zot_pdf (concat (getenv "yynote") "/zotero-pdf/"))
;;   (setq org_refs (concat (getenv "yynote") "/foam-template-master/masscode/org-mode/ref/"))
;;         ))
