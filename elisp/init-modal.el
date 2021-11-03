;;; -*- lexical-binding: t -*-

(straight-use-package '(meow :type git :host github :repo "DogLooksGood/meow"))

(defun meow-setup ()
  ;; Programmer Dvorak layout on ansi keyboard
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi
        meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  ;; it's not a good idea to have a complex leader keymap
  ;; here we create bindings for necessary, high frequency commands
  (meow-leader-define-key
   ;; reverse command query
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
   '("&" . +toggle-wide-tall-font)
   '(";" . comment-dwim)
   '("k" . kill-this-buffer)
   '("d" . dired)
   '("b" . switch-to-buffer)
   '("r" . rg-project)
   '("f" . find-file)
   '("i" . imenu)
   '("a" . "M-x")
   '("=" . "C-c ^")
   '("p" . project-find-file)
   '("t" . project-switch-to-buffer)
   '("l" . "C-x p p")
   '("y" . "C-x g")
   '("n" . "C-x M-n")
   ;; toggles
   '("L" . display-line-numbers-mode)
   '("S" . smartparens-strict-mode)
   '("T" . telega)
   '("P" . pass)
   '("A" . org-agenda)
   '("D" . docker)
   '("E" . elfeed)
   '("F" . flymake-mode)
   '("\\" . dired-sidebar-toggle-sidebar))
  (meow-motion-overwrite-define-key
   '("'" . repeat))
  (meow-normal-define-key
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-C-d)
   '("D" . meow-backspace)
   '("e" . meow-line)
   '("E" . meow-kmacro-lines)
   '("f" . meow-find)
   '("F" . meow-till)
   '("g" . meow-cancel)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("J" . meow-end-or-call-kmacro)
   '("k" . meow-kill)
   '("K" . meow-start-kmacro-or-insert-counter)
   '("l" . recenter-top-bottom)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("S" . meow-pop-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("Y" . meow-yank-pop)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("@" . recenter-top-bottom)
   '("'" . repeat)
   '("=" . align-regexp)
   '("<escape>" . meow-last-buffer)
   '("\\" . quoted-insert)
   '("?" . meow-keypad-describe-key)))

(setq
 meow-visit-sanitize-completion nil
 meow-esc-delay 0.001
 meow-keypad-describe-delay 0.5
 meow-select-on-change t
 meow-selection-command-fallback '((meow-replace . meow-page-up)
                                   (meow-change . meow-change-char)
                                   (meow-save . meow-save-empty)
                                   (meow-kill . meow-C-k)
                                   (meow-cancel . keyboard-quit)
                                   (meow-pop-selection . meow-pop-grab)
                                   (meow-delete . meow-C-d))
 meow-replace-state-name-list '((normal . "N")
                                (motion . "M")
                                (keypad . "K")
                                (insert . "I")))

(require 'meow)

(meow-global-mode 1)

(setq meow-cursor-type-keypad 'box)
(setq meow-cursor-type-insert '(bar . 3))

(with-eval-after-load "meow"
  ;; make Meow usable in TUI Emacs
  (meow-esc-mode 1)
  (add-to-list 'meow-mode-state-list '(inf-iex-mode . normal))
  (add-to-list 'meow-mode-state-list '(authinfo-mode . normal))
  (add-to-list 'meow-mode-state-list '(Custom-mode . normal))
  (add-to-list 'meow-mode-state-list '(cider-test-report-mode . normal))
  (add-to-list 'meow-mode-state-list '(cargo-process-mode . normal))
  (setq meow-grab-fill-commands nil)
  ;; use << and >> to select to bol/eol
  (add-to-list 'meow-char-thing-table '(?> . line))
  (add-to-list 'meow-char-thing-table '(?< . line))
  ;; define our command layout
  (meow-setup))

(provide 'init-modal)
