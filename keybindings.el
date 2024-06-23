;; https://www.spacemacs.org/doc/LAYERS.html#keybindingsel
;; keybindings.el
(with-eval-after-load 'org
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "of" 'yy-python-orgfind
    "os" 'yy-python-splittable
    "ol" 'yy-open-file-from-current-line
    "oo" 'yy-python-orgfind-and-splittable
    "or" 'my-org-rename-16
    ))

(dolist (mode '(python-mode
                emacs-lisp-mode
                sh-mode
                ;; 添加其他你想要支持的编程语言模式
                ))
  (spacemacs/set-leader-keys-for-major-mode mode
    "os" 'yy-python-searchcodeplus
    "oh" 'yy-highlight-lines-in-db
    "op" 'yy-python-searchcodeplus-and-highlight
    "oc" 'yy-process-and-copy-filename-code-to-org 
    "oj" 'yy-jump-to-line-in-db
    ))

