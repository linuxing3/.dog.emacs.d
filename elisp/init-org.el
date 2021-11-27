;;; -*- lexical-binding: t -*-

;;; packages to install
(straight-use-package 'htmlize)
(straight-use-package 'org-roam)
(straight-use-package 'org-superstar)
(straight-use-package 'ob-restclient)
(straight-use-package '(org-html-themify
                        :type git
                        :host github
                        :repo "DogLooksGood/org-html-themify"
                        :files ("*.el" "*.js" "*.css")))

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

;;; private config
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
  (setq org-clock-sound "~/org/music-box.wav"))

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
  (defun linuxing3/verify-refile-target ()
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'linuxing3/verify-refile-target))

(defun +org-init ()
  (variable-pitch-mode 1))

;;; Bootstrap
(with-eval-after-load  "org"
  (require 'org-tempo)
  (require 'org-protocol)
  (+org-babel-setup)

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
  (require 'ob-js)
  (linuxing3/org-config-h)
  (linuxing3/refile-config-h)
  (linuxing3/appearance-config-h)
  )

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

(require 'org-html-themify)
(add-hook 'org-mode-hook 'org-html-themify-mode)
(setq org-html-themify-themes '((dark . graverse)
                                (light . grayscale)))
;;; Application
(require 'org+roam)                     ; v2
(require 'org+latex)                    ; latex
(require 'org+capture)                  ; my capture template
(require 'org+agenda)                   ; super agenda support
(require 'org+pretty)                   ; pretty symbols
(require 'org+publish)                  ; publish to server
(require 'org+block)                    ; abbreviation template

(provide 'init-org)
