;;; org-roam v2

(setq
 org-roam-v2-ack t
 org-roam-directory
 (let ((p (expand-file-name "~/org/roam")))
   (unless (file-directory-p p) (make-directory p))
   p))

(with-eval-after-load "org-roam"
  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (global-set-key (kbd "C-x M-n l") 'org-roam-buffer-toggle)
  (global-set-key (kbd "C-x M-n f") 'org-roam-node-find)
  (global-set-key (kbd "C-x M-n g") 'org-roam-graph)
  (global-set-key (kbd "C-x M-n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-x M-n c") 'org-roam-capture)
  (global-set-key (kbd "C-x M-n s") 'org-roam-db-sync)

  (org-roam-setup)
  (require 'org-roam-protocol))

;;; templates
(with-eval-after-load "org-roam"
  ;; 自定义私人笔记标题的处理方法
  (defun linuxing3/org-roam-title-private (title)
    (let ((timestamp (format-time-string "%Y-%m-%d" (current-time)))
          (slug (org-roam--title-to-slug title)))
      (format "%s-%s" timestamp slug)))
  ;; `file' 日记模板 - diaries/2021-11-10.org
  (setq org-roam-dailies-capture-templates
        '(("d" "默认" entry #'org-roam-capture--get-point "* %?"
           :target (file "daily/%<%Y-%m-%d>" ;; `headline1')
           :head "#+title: \n#+date: %<%Y-%m-%d-%Z>\n"
           :unnarrowed t)
          ("x" "个人" entry #'org-roam-capture--get-point "* %?"
           :target (file "daily/%<%Y-%m-%d>" ;; `headline1')
           :head "#+title: \n#+date: %<%Y-%m-%d-%Z>\n"
           :unnarrowed t)
          ("t" "任务" entry
           #'org-roam-capture--get-point
           "* [待办] %?\n  %U\n  %a\n  %i"
           :target (file+olp "daily/%<%Y-%m-%d>" "Tasks")
           :empty-lines 1
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("j" "日记" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
           :target (file+olp "daily/%<%Y-%m-%d>" "Log")
           :head "#+title:  %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("l" "日志" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - %?"
           :target (file+olp "daily/%<%Y-%m-%d>" "Log")
           :head "#+title:  %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("m" "会议" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
           :target (file+olp "daily/%<%Y-%m-%d>" "Log")
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  ;; `file' 自定义笔记模板 - 2021-11-10--file-title.org
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :target (file "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)")
           :head "#+title: ${title}\n#+date: ${time} \n#+roam_alias: \n#+roam_tags: \n"
           :unnarrowed t)
          ))
  ;; `file' 专业术语模板 - 202111101000000-title.org
  (add-to-list 'org-roam-capture-templates
               '("t" "Term" plain (function org-roam-capture--get-point)
                 "- 领域: %^{术语所属领域}\n- 释义:"
                 :target (file "%<%Y%m%d%H%M%S>-${slug}")
                 :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags: \n\n"
                 :unnarrowed t
                 ))
  ;; `file' 工作试验模板 - 202111101000000-title.org
  (add-to-list 'org-roam-capture-templates
               '("p" "Paper Note" plain (function org-roam-capture--get-point)
                 "* 相关工作\n\n%?\n* 观点\n\n* 模型和方法\n\n* 实验\n\n* 结论\n"
                 :target (file "%<%Y%m%d%H%M%S>-${slug}")
                 :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags: \n\n"
                 :unnarrowed t
                 ))
  ;; `immediate' 即可输入标题和日期，生成一个笔记
  (setq org-roam-capture-immediate-template
        '("d" "default" plain (function org-roam-capture--get-point)
          "%?"
          :target (file "%<%Y%m%d%H%M%S>-${slug}")
          :head "#+title: ${title}\n#+date: ${date}"
          :unnarrowed t))
  ;; `ref' 抓取网页书签到一个用网页标题命名的文件中
  (setq org-roam-capture-ref-templates
        '(("pb" "ref" plain (function org-roam-capture--get-point)
           ""
           :target (file "${slug}")
           :head "\n#+title: ${title}\n#+roam_key: ${ref}\n"
           :unnarrowed t)))
  ;; `content'  抓取一个网页中的内容，多次分别插入到用网页标题命名的文件中
  (add-to-list 'org-roam-capture-ref-templates
               '("pa" "Annotation" plain (function org-roam-capture--get-point)
                 "** %U \n${body}\n"
                 :target (file "${slug}")
                 :head "\n#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
                 :immediate-finish t
                 :unnarrowed t)))

(require 'org-roam)

(provide 'org+roam)
