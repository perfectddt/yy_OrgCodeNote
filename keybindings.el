;; https://www.spacemacs.org/doc/LAYERS.html#keybindingsel
;; keybindings.el
;; (spacemacs/declare-prefix "n" "notes")
;; (spacemacs/set-leader-keys
;;   ;; org-roam
;;   "nf" 'org-roam-node-find
;;   "ni" 'org-roam-node-insert
;;   "nc" 'org-roam-capture
;;   "nl" 'org-roam-buffer-toggle ; 显示后链窗
;;   "nu" 'org-roam-ui-mode; 浏览器中可视化
;;   "nd" 'org-roam-dailies-map; 日记菜单
;;   ;;org-roam-bibtex
;;   "nk" 'orb-insert-link
;;   "na" 'orb-note-action)
(with-eval-after-load 'org
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "of" 'yy-python-orgfind
    "os" 'yy-python-splittable
    "ol" 'yy-open-file-from-current-line
    "oo" 'yy-python-orgfind-and-splittable
    ))
(dolist (mode '(python-mode
                emacs-lisp-mode
                ;; 添加其他你想要支持的编程语言模式
                ))
  (spacemacs/set-leader-keys-for-major-mode mode
    "os" 'yy-python-searchcodeplus
    "oh" 'yy-highlight-lines-in-db
    "op" 'yy-python-searchcodeplus-and-highlight
    "oc" 'yy-process-and-copy-filename-code-to-org 
    ))

