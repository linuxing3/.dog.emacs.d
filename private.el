
(straight-use-package '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(straight-use-package '(mu4e-dashboard :type git :host github :repo "rougier/mu4e-dashboard"))
(straight-use-package 'atom-one-dark-theme)
(straight-use-package 'doom-themes)
(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)

(setq +font-family "Droid Sans Mono"
      +font-unicode-family "WenQuanYi Micro Hei Mono"
      +variable-pitch-family "Cantarell"
      +fixed-pitch-family "Droid Sans Mono"
      +font-size 16
      +font-rescale '((tall . 1.2) (wide . 1.2))
      +theme-list '(atom-one-dark joker))
