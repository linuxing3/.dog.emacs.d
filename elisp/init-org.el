;;; -*- lexical-binding: t -*-

(straight-use-package 'htmlize)
(straight-use-package 'org-roam)
(straight-use-package 'org-superstar)
(straight-use-package 'ob-restclient)
(straight-use-package '(org-html-themify
                        :type git
                        :host github
                        :repo "DogLooksGood/org-html-themify"
                        :files ("*.el" "*.js" "*.css")))

;;; Latex support
;;; install latex with
;;; pacman -S texlive-bin texlive-most
;;; install xdot
;;; pacman -S xdot

(defvar-local +org-last-in-latex nil)

(defun +org-post-command-hook ()
  (ignore-errors
    (let ((in-latex (and (derived-mode-p  'org-mode)
                         (or (org-inside-LaTeX-fragment-p)
                             (org-inside-latex-macro-p)))))
      (if (and +org-last-in-latex (not in-latex))
          (progn (org-latex-preview)
                 (setq +org-last-in-latex nil)))

      (when-let ((ovs (overlays-at (point))))
        (when (->> ovs
                   (--map (overlay-get it 'org-overlay-type))
                   (--filter (equal it 'org-latex-overlay)))
          (org-latex-preview)
          (setq +org-last-in-latex t)))

      (when in-latex
        (setq +org-last-in-latex t)))))

(define-minor-mode org-latex-auto-toggle
  "Auto toggle latex overlay when cursor enter/leave."
  :init-value nil
  :keymap nil
  :lighter nil
  (if org-latex-auto-toggle
      (add-hook 'post-command-hook '+org-post-command-hook nil t)
    (remove-hook 'post-command-hook '+org-post-command-hook t)))

;;; Update latex options after change theme.

(defun +org-update-latex-option-by-theme (theme)
  (when (bound-and-true-p org-format-latex-options)
    (setq org-format-latex-options
          (plist-put org-format-latex-options :theme theme))))

(add-hook '+after-change-theme-hook '+org-update-latex-option-by-theme)

;;; Org babel

(defun +org-redisplay-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(defun +org-babel-setup ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook '+org-redisplay-inline-images))

;;; org-mode

(setq org-html-checkbox-type 'unicode)

;;; Init config
(defun linuxing3/org-config-h()
  ;; 设定`todo关键字'
  (setq org-todo-keywords '((sequence "[学习](s)" "[待办](t)" "[等待](w)" "|" "[完成](d)" "[取消](c)")
                            (sequence "[BUG](b)" "[新事件](i)" "[已知问题](k)" "[修改中](W)" "|" "[已修复](f)")))

  ;; 设定`agenda相关目录'
  (setq diary-file "~/org/diary")
  (setq org-agenda-diary-file "~/org/diary")

  (setq org-agenda-files (directory-files org-directory t "\\.agenda\\.org$" t))

  (setq org-archive-location "~/org/archived/%s_archive::")
  (setq org-return-follows-link t)
  ;; onekey trigger state
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-timer-default-timer 25)
  (setq org-clock-sound "~/.evil.emacs.d/assets/music/music-box.wav"))

;;; Styles
(defun linuxing3/appearance-config-h ()
  "Configures the UI for `org-mode'."

  (setq org-ellipsis " ▼ "
        org-bullets-bullet-list '(" ○ " " ◆ ")
        org-tags-column -80)

  (setq org-todo-keyword-faces
        '(
          ("[学习]" . (:foreground "GoldenRod" :weight bold))
          ("[待办]" . (:foreground "IndianRed1" :weight bold))
          ("[等待]" . (:foreground "OrangeRed" :weight bold))
          ("[完成]" . (:foreground "coral" :weight bold))
          ("[取消]" . (:foreground "LimeGreen" :weight bold))
          ("[BUG]" . (:foreground "GoldenRod" :weight bold))
          ("[新事件]" . (:foreground "IndianRed1" :weight bold))
          ("[已知问题]" . (:foreground "OrangeRed" :weight bold))
          ("[修改中]" . (:foreground "coral" :weight bold))
          ("[已修复]" . (:foreground "LimeGreen" :weight bold))
          ))

  ;; FIXME: 如果启用自定义时间格式，将无法在时间内部进行修改
  ;; (setq-default org-display-custom-times t)
  ;; (setq org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  (setq org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-image-actual-width nil
        org-imenu-depth 6
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{}
        org-startup-folded nil)
  (setq org-reverse-note-order t))

(defun linuxing3/refile-config-h ()
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache nil)
  (setq org-blank-before-new-entry nil)
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
					; Allow refile to create parent tasks with confirmation
  (defun linuxing3/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'linuxing3/verify-refile-target))

(defun +org-init ()
  (progn
    (variable-pitch-mode 1)
    (linuxing3/org-config-h)
    (linuxing3/refile-config-h)
    (linuxing3/appearance-config-h)))

(with-eval-after-load  "org"
  (define-key org-mode-map (kbd "<f8>") 'org-latex-auto-toggle)
  (require 'org-tempo)
  (+org-babel-setup)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 4.0)
        org-agenda-files '("~/org"))
  (custom-set-faces
   '(org-table ((t :inherit 'fixed-pitch)))
   '(org-code ((t :inherit 'fixed-pitch)))
   '(org-block ((t :inherit 'fixed-pitch)))
   '(org-checkbox ((t :inherit 'fixed-pitch))))

  (add-hook 'org-mode-hook #'+org-init)
  (require 'ob)
  (require 'ob-dot)
  (require 'ob-plantuml)
  (require 'ob-restclient)
  (require 'ob-clojure)
  (require 'ob-js))

(with-eval-after-load "ob"
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)
     (plantuml . t)
     (restclient . t)
     (python . t)
     (clojure . t)
     (R . t)
     (shell . t))))

;;; org-roam

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
           :file-name "daily/%<%Y-%m-%d>" ;; `headline1'
           :head "#+title: \n#+date: %<%Y-%m-%d-%Z>\n"
           :unnarrowed t)
          ("x" "个人" entry #'org-roam-capture--get-point "* %?"
           :file-name "daily/%<%Y-%m-%d>" ;; `headline1'
           :head "#+title: \n#+date: %<%Y-%m-%d-%Z>\n"
           :unnarrowed t)
          ("t" "任务" entry
           #'org-roam-capture--get-point
           "* [待办] %?\n  %U\n  %a\n  %i"
           :file-name "daily/%<%Y-%m-%d>"
           :olp ("Tasks") ;; under `Task' subtree
           :empty-lines 1
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("j" "日记" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
           :file-name "daily/%<%Y-%m-%d>"
           :olp ("Log") ;; under `Log' subtree
           :head "#+title:  %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("l" "日志" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - %?"
           :file-name "daily/%<%Y-%m-%d>"
           :olp ("Log") ;; under `Log' subtree
           :head "#+title:  %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("m" "会议" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
           :file-name "daily/%<%Y-%m-%d>"
           :olp ("Log") ;; under `Log' subtree
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  ;; `file' 自定义笔记模板 - 2021-11-10--file-title.org
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
           :head "#+title: ${title}\n#+date: ${time} \n#+roam_alias: \n#+roam_tags: \n"
           :unnarrowed t)
          ))
  ;; `file' 专业术语模板 - 202111101000000-title.org
  (add-to-list 'org-roam-capture-templates
               '("t" "Term" plain (function org-roam-capture--get-point)
                 "- 领域: %^{术语所属领域}\n- 释义:"
                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
                 :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags: \n\n"
                 :unnarrowed t
                 ))
  ;; `file' 工作试验模板 - 202111101000000-title.org
  (add-to-list 'org-roam-capture-templates
               '("p" "Paper Note" plain (function org-roam-capture--get-point)
                 "* 相关工作\n\n%?\n* 观点\n\n* 模型和方法\n\n* 实验\n\n* 结论\n"
                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
                 :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags: \n\n"
                 :unnarrowed t
                 ))
  ;; `immediate' 即可输入标题和日期，生成一个笔记
  (setq org-roam-capture-immediate-template
        '("d" "default" plain (function org-roam-capture--get-point)
          "%?"
          :file-name "%<%Y%m%d%H%M%S>-${slug}"
          :head "#+title: ${title}\n#+date: ${date}"
          :unnarrowed t))
  ;; `ref' 抓取网页书签到一个用网页标题命名的文件中
  (setq org-roam-capture-ref-templates
        '(("pb" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "\n#+title: ${title}\n#+roam_key: ${ref}\n"
           :unnarrowed t)))
  ;; `content'  抓取一个网页中的内容，多次分别插入到用网页标题命名的文件中
  (add-to-list 'org-roam-capture-ref-templates
               '("pa" "Annotation" plain (function org-roam-capture--get-point)
                 "** %U \n${body}\n"
                 :file-name "${slug}"
                 :head "\n#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
                 :immediate-finish t
                 :unnarrowed t)))


(require 'org-roam)

(require 'org-html-themify)
(add-hook 'org-mode-hook 'org-html-themify-mode)
(setq org-html-themify-themes '((dark . graverse)
                                (light . grayscale)))
;;; Customization
(require 'init-org-plus)
(require 'org+publish)
(require 'org+reminder)
(require 'org+pretty)

(provide 'init-org)
