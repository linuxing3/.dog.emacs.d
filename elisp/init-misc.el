(straight-use-package 'dumb-jump)
(straight-use-package 'pinentry)
(straight-use-package 'highlight-numbers)
(straight-use-package 'rainbow-mode)
(straight-use-package 'multiple-cursors)

(require 'dumb-jump)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; (add-hook 'prog-mode-hook 'eldoc-box-hover-mode)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; ~/.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry

(pinentry-start)

(require 'colorful)
(add-hook 'prog-mode-hook 'colorful-mode)

;; (require 'htab)
;; (htab-global-mode 1)
;; (add-to-list 'htab-ignore-commands 'meow-minibuffer-quit)
;; (global-set-key (kbd "<XF86Forward>") #'htab-next-buffer)
;; (global-set-key (kbd "<XF86Back>") #'htab-prev-buffer)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

(provide 'init-misc)
