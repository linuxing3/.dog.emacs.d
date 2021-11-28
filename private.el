(straight-use-package '(elegant-emacs :type git :host github :repo "rougier/elegant-emacs"))
(straight-use-package '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(straight-use-package '(mu4e-dashboard :type git :host github :repo "rougier/mu4e-dashboard"))
(straight-use-package 'doom-themes)
(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)

(setq +font-family "Roboto Mono"
      +font-unicode-family "WenQuanYi Micro Hei Mono"
      +variable-pitch-family "Cantarell"
      +fixed-pitch-family "Roboto Mono"
      +font-size 16
      +theme-list '(doom-palenight doom-one-light joker))
