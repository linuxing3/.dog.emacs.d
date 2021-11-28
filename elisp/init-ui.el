;; themes and toolbar
(defvar efs/default-font-size 140)
(defvar efs/default-variable-font-size 140)
(defvar efs/frame-transparency '(90 . 90))

(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; font
(defun +modern-ui-font-h ()
  (progn
    (set-face-attribute 'default nil :font "Roboto Mono" :height efs/default-font-size)
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font "Roboto Mono" :height efs/default-font-size)
    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)
    ))

(defun +modern-ui-chinese-h ()
  "Set Font for chinese language"
  (set-fontset-font
   t
   'han
   (cond
    ((string-equal system-type "windows-nt")
     (cond
      ((member "Microsoft YaHei UI" (font-family-list)) "Microsoft YaHei UI")
      ))
    ((string-equal system-type "darwin")
     (cond
      ((member "Hei" (font-family-list)) "Hei")
      ((member "Heiti SC" (font-family-list)) "Heiti SC")
      ((member "Heiti TC" (font-family-list)) "Heiti TC")))
    ((string-equal system-type "gnu/linux")
     (cond
      ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei"))))))

;;; No scroll bar
(defun +modern-ui-slim-h ()
    (progn
      (setq-default scroll-bar-width 10)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (window-divider-mode 1)
      (blink-cursor-mode -1)))

(require 'editor+eshell)                ; git eshell

;; (require 'elegant)
;; (require 'elegance)
;; (require 'sanity)
;; (load-theme 'elegant-light)
;; (load-theme 'doom-palenight)

(when (display-graphic-p)
  (+modern-ui-font-h)
  (+modern-ui-chinese-h)
  (+modern-ui-slim-h))

(provide 'init-ui)
