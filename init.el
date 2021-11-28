(require 'init-util)
(require 'init-modal)                   ; moew mode
(require 'init-modal-qwerty)            ; moew mode, one keyboard

(require 'init-project)
(require 'init-edit)
(require 'init-dired)
(require 'editor+eshell)                ; git eshell

(require 'init-lsp)                     ; IDE support
(require 'init-web)
;; (require 'init-clojure)
;; (require 'init-elixir)
(require 'init-python)
;; (require 'init-haskell)
(require 'init-rust)
(require 'init-typescript)
(require 'init-markdown)
(require 'init-git)

(require 'init-org)                     ; org mode productivity

(require 'init-completion)              ; selectrum/consult/prescient
(require 'init-pass)
(require 'init-rime)
(require 'init-conf)
(require 'init-window)
;; (require 'init-docker)
(require 'init-modeline)
(require 'init-telega)
(require 'init-misc)
(require 'init-direnv)
(require 'init-restclient)
(unless window-system
  (require 'init-xterm))
(require 'init-rss)
(require 'init-server)

(require 'init-format)                  ; format all
(require 'init-keybinds)                ; leader keybinds with f7
;; (require 'init-nano)                 ; nano theme
