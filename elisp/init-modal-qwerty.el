;;; -*- lexical-binding: t -*-


(straight-use-package '(meow :type git :host github :repo "DogLooksGood/meow"))

;;; KEYS STILL WORK
;;; c-n/p       ; move line
;;; c-f/b       ; move character
;;; A-M-> <     ; begin end of buffer          | gg
;;; xKp         ; delete a line and paste      | ddp
;;; xyp         ; duplicate a line and paste   | yyp

(meow-leader-define-key
   ;; reverse command query
   '("^" . meow-keypad-describe-key)
   ;; cheatsheet
   '("?" . meow-cheatsheet)
   ;; high frequency keybindings
   '("e" . "C-x C-e")
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("," . "M-,")
   ;; window management
   '("w" . other-window)
   '("W" . window-swap-states)
   '("o" . delete-other-windows)
   '("s" . split-window-right)
   '("-" . split-window-below)
   ;; high frequency commands
   '("$" . +change-theme)
   '(";" . comment-dwim)                ; comment a line
   '("k" . kill-this-buffer)
   '("p" . project-find-file)
   '("j" . project-switch-to-buffer)
   '("d" . dired)
   '("b" . counsel-buffer)
   '("r" . rg-project)
   '("f" . find-file)
   '("i" . imenu)
   '("a" . "M-x")
   '("v" . "C-x g")
   ;; toggles
   '("L" . display-line-numbers-mode)
   '("S" . smartparens-strict-mode)
   '("t" . telega)
   '("P" . pass)
   '("R" . org-roam-mode)
   '("A" . org-agenda)
   '("C" . org-capture)
   '("D" . docker))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  ;;; `SPC' as leader key
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   ;; cheatsheet
   '("?" . meow-cheatsheet)
   ;; high frequency keybindings
   '("e" . "C-x C-e")                   ; evaluate last expression
   '(")" . "C-)")                       ; indent next line with )
   '("}" . "C-}")                       ; close previous )
   '("." . "M-.")                       ; jump to definition
   '("," . "M-,")                       ; jump back from definition
   ;; window management
   '("w" . other-window)                ; choose other window quickly
   '("W" . window-swap-states)          ; swap
   '("o" . delete-other-windows)        ; delete other window
   '("s" . split-window-right)          ; split vertically
   '("-" . split-window-below)          ; split horizontally
   ;; high frequency commands
   '("$" . +change-theme)
   '(";" . comment-dwim)                ; comment a line
   '("k" . kill-this-buffer)            ; kill buffer
   '("p" . project-find-file)           ; find in project dir
   '("j" . project-switch-to-buffer)    ; switch to buffer in project dir
   '("d" . dired)                       ; directory explorer
   '("b" . switch-to-buffer)            ; switch buffer
   '("r" . rg-project)                  ; rg PATTERN in dir
   '("f" . find-file)                   ; open file
   '("i" . imenu)                       ; imenu to choose from
   '("a" . "M-x")                       ; app / extended command
   '("v" . "C-x g")                     ; magit status
   ;; toggles
   '("L" . display-line-numbers-mode)   ; line number toggle
   '("S" . smartparens-strict-mode)     ; parentasis guard
   '("t" . telega)
   '("P" . pass)
   '("R" . org-roam-mode)               ; roam mode
   '("A" . org-agenda)                  ; agenda in org
   '("C" . org-capture))                ; quickly capture in org
   '("D" . docker)
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   ;;; work on `THINGS'
   '("," . meow-inner-of-thing)
   '("'" . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   ;;; open and insert
   '("i" . meow-insert)                 ; insert
   '("I" . meow-open-above)             ; open a line above
   '("a" . meow-append)                 ; append after
   '("A" . meow-open-below)             ; open a line below
   ;;; find and select
   '("c" . meow-change)                 ; change a word
   '("C" . meow-change-save)
   '("d" . meow-C-d)                    ; delete one character
   '("w" . meow-mark-word)              ; mark a whole word under cursor (a)
   '("W" . meow-mark-symbol)            ; mark a whole word-sequence under cursor (a-b-c-d)
   '("x" . meow-line)                   ; mark the whole line, xyp duplicate a line, xKp move a line
   '("X" . meow-kmacro-lines)
   '("D" . meow-backward-delete)        ; delete backward a charater
   ;;; action
   '("f" . meow-find)                   ; find a character
   '("F" . meow-find-expand)            ; TODO: find expand?
   '("g" . meow-cancel)
   '("G" . meow-grab)                   ; Grap as `secondary' selection, R to swap
   '("R" . meow-swap-grab)              ; swap with the secondary selection
   '("Y" . meow-sync-grab)              ; sync with the secondary selection
   ;;; `hjkl' direction motion
   '("h" . meow-left)                   ; <--
   '("H" . meow-left-expand)
   '("j" . meow-next)                   ;
   '("M" . meow-next-expand)            ; J --> M
   '("k" . meow-prev)
   '("N" . meow-prev-expand)            ; K --> N
   '("l" . meow-right)                  ; -->
   ;;; `word' and `symbol' Motion
   '("b" . meow-back-word)              ; move back a word
   '("B" . meow-back-symbol)            ; move back a sequence of word
   '("e" . meow-next-word)              ; move from cursor to tail of word(a)
   '("E" . meow-next-symbol)            ; move to next symbol (a-b-c-c)
   '("L" . meow-right-expand)
   ;;; `extra' act on `selection'
   '("J" . meow-join)                   ; join
   '("/" . meow-search)                 ; search match as selection
   '("N" . meow-pop-search)
   '("o" . meow-block)                  ; select block in parentasis
   '("O" . meow-block-expand)
   ;;; `delete', `copy' and `paste'
   '("K" . meow-kill)                   ; kill until end
   '("t" . meow-till)                   ; go until find match
   '("y" . meow-save)                   ; save to register
   '("p" . meow-yank)                   ; paste from register
   '("P" . meow-yank-pop)
   ;;; misc
   '("q" . meow-quit)
   '("Q" . meow-goto-line)              ; go to a line with number
   '("r" . meow-replace)                ; replace
   '("T" . meow-till-expand)            ; go until expand
   '("u" . meow-undo)                   ; cancel operation
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)                  ; vertically visit a MATCH
   '("V" . meow-kmacro-matches)
   '("z" . meow-pop-selection)          ; cancel selection region
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)          ; search and replace
   '("%" . meow-query-replace-regexp)
   '("." . repeat)                      ; repeat last action'("\\" . quoted-insert)
   '("<escape>" . meow-last-buffer)))   ; switch to last buffer

(setq
 meow-visit-sanitize-completion nil
 meow-esc-delay 0.001
 meow-keypad-describe-delay 0.5
 meow-select-on-change t
 meow-cursor-type-normal 'box
 meow-cursor-type-insert '(bar . 4)
 meow-selection-command-fallback '((meow-replace . meow-page-up)
                                   (meow-change . meow-change-char)
                                   (meow-save . meow-save-empty)
                                   (meow-kill . meow-C-k)
                                   (meow-cancel . keyboard-quit)
                                   (meow-pop . meow-pop-grab)
                                   (meow-delete . meow-C-d)))

(require 'meow)

(meow-global-mode 1)

(with-eval-after-load "meow"
  ;; make Meow usable in TUI Emacs
  (meow-esc-mode 1)
  (add-to-list 'meow-mode-state-list '(inf-iex-mode . normal))
  (add-to-list 'meow-mode-state-list '(authinfo-mode . normal))
  (add-to-list 'meow-mode-state-list '(Custom-mode . normal))
  (add-to-list 'meow-mode-state-list '(cider-test-report-mode . normal))
  (add-to-list 'meow-grab-fill-commands 'eval-expression)
  (setq meow-cursor-type-keypad 'box)
  (setq meow-cursor-type-insert '(bar . 2))
  ;; use << and >> to select to bol/eol
  (add-to-list 'meow-char-thing-table '(?> . line))
  (add-to-list 'meow-char-thing-table '(?< . line))
  ;; define our command layout
  (meow-setup)
  ;; add indicator to modeline
  (meow-setup-indicator))

(provide 'init-modal-qwerty)
