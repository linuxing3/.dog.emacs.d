;;; -*- lexical-binding: t -*-

(straight-use-package 'dash)

(require 'dash)
(require 'subr-x)

;; 常用变量
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defvar-local +project-name-cache nil
  "Cache for current project name.")
;;
;;; git sync
;;;###autoload
(defun +git-push(dir)
  "Run git push in a specific directory"
  (interactive)
  (with-dir dir
            (shell-command "git add .")
            (--> (format-time-string "%Y-%m-%d %H:%M" (current-time))
                 (concat "git commit -m \"" it "\"")
                 (shell-command it))
            (shell-command "git push")))

;;
;;; git sync
;;;###autoload
(defun +git-pull(dir)
  "Run git push in a specific directory"
  (interactive)
  (with-dir dir
            (shell-command "git pull")))

;;
;;; ensure package to be installed

(defun +ensure-package (LIST)
  "Ensure package is installed"
  (when (cl-find-if-not #'package-installed-p LIST)
    ;; (package-refresh-contents)
    (mapc #'package-install LIST)))


(defun linuxing3/reload-emacs ()
  "Ensure package is installed"
  (interactive)
  (load "~/.emacs.d/init.el"))


(defun +in-string-p ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (or (nth 3 (syntax-ppss))
      (member 'font-lock-string-face
              (text-properties-at (point)))))

(defun +in-comment-p ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (nth 4 (syntax-ppss)))

(defvar +smart-file-name-cache nil)

(defun +shorten-long-path (path)
  (let ((paths (split-string path "/")))
    (if (< (length paths) 3)
        path
      (string-join (reverse (let ((rpaths (reverse paths)))
                                (-concat
                                 (-take 2 rpaths)
                                 (->> (-drop 2 rpaths)
                                      (--map (if (> (length it) 1)
                                                 (substring it 0 1)
                                               it))))))
                     "/"))))

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (concat
       (propertize
        (car
         (reverse
          (split-string (string-trim-right vc-dir "/") "/")))
        'face
        'bold)
       "/"
       (+shorten-long-path (file-relative-name bfn vc-dir))))
     (bfn bfn)
     (t (buffer-name)))))

(defun +smart-file-name-cached ()
  (if (eq (buffer-name) (car +smart-file-name-cache))
      (cdr +smart-file-name-cache)
    (let ((file-name (+smart-file-name)))
      (setq +smart-file-name-cache
            (cons (buffer-name) file-name))
      file-name)))

(defun +vc-branch-name ()
  (when vc-mode
    (propertize
     (replace-regexp-in-string
      "Git[-:]"
      ""
      (substring-no-properties vc-mode))
     'face
     'bold)))

(defmacro +measure-time-1 (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.03fms"
              (* 1000 (float-time (time-since time))))))

(defmacro +measure-time (&rest body)
  "Measure the time it takes to evalutae BODY, repeat 10 times."
  `(let ((time (current-time))
         (n 10))
     (dotimes (_ n),@body)
     (message "%.03fms"
              (/ (* (float-time (time-since time)) 1000) n))))

(defface +modeline-dim-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey50")))
  "Dim face in mode-line")

(defun +make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

;;; Case transform

(defun +to-pascal-case (s)
  (let* ((words (split-string s "-\\|_"))
         (capwords (mapcar #'capitalize words)))
    (string-join capwords "")))

(defun +color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (ignore-errors
    (apply #'(lambda (r g b)
               format "#%02x%02x%02x"
               (ash r -8)
               (ash g -8)
               (ash b -8))
           (cl-mapcar
            (lambda (x y)
              (round (+ (* x alpha) (* y (- 1 alpha)))))
            (color-values c1) (color-values c2)))))

(defun +set-no-other-window ()
  (set-window-parameter (car (window-list)) 'no-other-window t))

;; ===============================================
;; Autoloads
;; ===============================================
;;;###autoload
(defmacro with-dir (DIR &rest FORMS)
  "Execute FORMS in specific DIR."
  (let ((orig-dir (gensym)))
    `(prog2
         (setq ,orig-dir default-directory)
         (progn (cd ,DIR) ,@FORMS)
       (cd ,orig-dir))))
;; (macroexpand '(with-dir "~/.emacs.d"))

;;;###autoload
(defun os-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
      (expand-file-name path home-directory)
    (expand-file-name path home-directory)))

;;;###autoload
(defun private-module-path (path)
  "Prepare module path"
  (expand-file-name path linuxing3-private-modules))

;;;###autoload
(defun dropbox-path (path)
  "Prepare cloud privider path"
  (expand-file-name path (concat home-directory cloud-service-provider)))

;;;###autoload
(defun workspace-path (path)
  "Prepare workspace path"
  (expand-file-name path (concat home-directory linuxing3-private-workspace)))

;; ===============================================
;; 核心设置
;; ===============================================

;;; 文件目录设置
(defgroup linuxing3 nil
  "Linuxinge Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/linuxing3/.dog.emacs.d"))

(defcustom linuxing3-logo (expand-file-name
                           (if (display-graphic-p) "logo.png" "banner.txt")
                           user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :group 'linuxing3
  :type 'string)

(defcustom linuxing3-full-name user-full-name
  "Set user full name."
  :group 'linuxing3
  :type 'string)

(defcustom linuxing3-mail-address user-mail-address
  "Set user email address."
  :group 'linuxing3
  :type 'string)

(defcustom home-directory (expand-file-name "~/")
  "Set home directory."
  :group 'linuxing3
  :type 'string)

(defcustom data-drive "/"
  "root directory of your personal data,
in windows could be c:/Users/Administrator"
  :group 'linuxing3
  :type 'string)

(defcustom cloud-service-provider "OneDrive"
  "Could be Dropbox o others, which will hold org directory etc"
  :group 'linuxing3
  :type 'string)

(defcustom linuxing3-private-modules "~/.evil.emacs.d/modules"
  "Normally I use EnvSetup directory to hold all my private lisp files"
  :group 'linuxing3
  :type 'string)

(defcustom linuxing3-private-workspace "workspace"
  "Normally I use workspace directory to hold all my private working projects"
  :group 'linuxing3
  :type 'string)

(defcustom linuxing3-completion-mode "vertico"
  "Can be vertico o ivy for completion"
  :group 'linuxing3
  :type 'string)


(provide 'init-util)
