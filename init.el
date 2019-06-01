;;; init.el --- yiufung's config  -*- lexical-binding: t; coding:utf-8; fill-column: 119 -*-

;;; Commentary:
;; My personal config. Use `outshine-cycle-buffer' (<S-Tab>) to navigate through sections, and `counsel-imenu' (C-c i)
;; to locate individual use-package definition.

;;; Bootstrap

;; Speed up bootstrapping
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 800000
                                    gc-cons-percentage 0.1)
                              (garbage-collect)) t)

;; Always follow symlinks. init files are normally stowed/symlinked.
(setq vc-follow-symlinks t
      find-file-visit-truename t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bootstrap `use-package'
(setq-default use-package-always-defer t ; Always defer load package to speed up startup time
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
;; Integration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; Early load Org from Git version instead of Emacs built-in version
(straight-use-package 'org-plus-contrib)

;;; Basic Setup
;;;; Emacs folder setup

;; Emacs configuration, along with many other journals, are synchronized across machines
(setq my-sync-directory "~/Dropbox")
;; Define configuration directory.
(setq my-emacs-conf-directory (expand-file-name "dotfiles/emacs/" my-sync-directory)
      my-private-conf-directory (expand-file-name "private/" my-emacs-conf-directory))
;; For packages not available through MELPA, save it locally and put under load-path
(add-to-list 'load-path (expand-file-name "elisp" my-emacs-conf-directory))

(use-package auth-source
  :straight ivy-pass
  ;; Setup Credentials
  :bind (("s-P" . ivy-pass))
  :config
  ;; Redefine some functions in auth-source-pass.el, because they don't respect
  ;; environment variable PASSWORD_STORE_DIR
  (defvar auth-source-pass--cache (make-hash-table :test #'equal))

  (defun auth-source-pass--reset-cache ()
    (setq auth-source-pass--cache (make-hash-table :test #'equal)))

  (defun auth-source-pass--read-entry (entry)
    "Return a string with the file content of ENTRY."
    (run-at-time 45 nil #'auth-source-pass--reset-cache)
    (let ((cached (gethash entry auth-source-pass--cache)))
      (or cached
          (puthash
           entry
           (with-temp-buffer
             (insert-file-contents (expand-file-name
                                    (format "%s.gpg" entry)
                                    (getenv "PASSWORD_STORE_DIR")))
             (buffer-substring-no-properties (point-min) (point-max)))
           auth-source-pass--cache))))

  (defun auth-source-pass-entries ()
    "Return a list of all password store entries."
    (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
      (mapcar
       (lambda (file) (file-name-sans-extension (file-relative-name file store-dir)))
       (directory-files-recursively store-dir "\.gpg$"))))

  ;; Enable password-store with auth-source
  ;; auth-source-pass-get is the main entrance.
  (auth-source-pass-enable)

  ;; To debug, set auth-source-debug to t.
  ;; Also use auth-source-forget-all-cached
  )


;;;; General settings
(setq-default ;; Use setq-default to define global default
 ;; Who I am
 user-mail-address "cheongyiufung@gmail.com"
 user-full-name "Cheong Yiu Fung"
 ;; Enable all disabled commands
 disabled-command-function nil
 ;; Don't show scratch message, and use fundamental-mode for *scratch*
 ;; Remove splash screen and the echo area message
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message 'nil
 initial-major-mode 'fundamental-mode
 ;; Emacs modes typically provide a standard means to change the
 ;; indentation width -- eg. c-basic-offset: use that to adjust your
 ;; personal indentation width, while maintaining the style (and
 ;; meaning) of any files you load.
 indent-tabs-mode nil ; don't use tabs to indent
 tab-width 8 ; but maintain correct appearance
 ;; Use one space as sentence end
 sentence-end-double-space 'nil
 ;; Newline at end of file
 require-final-newline t
 ;; Don't adjust window-vscroll to view tall lines.
 auto-window-vscroll nil
 ;; Don't create lockfiles.
 ;; recentf frequently prompts for confirmation.
 create-lockfiles nil
 ;; Move files to trash when deleting
 delete-by-moving-to-trash t
 ;; Show column number
 column-number-mode t
 ;; Don't break lines for me, please
 truncate-lines t
 ;; More message logs
 message-log-max 16384
 ;; No electric indent
 electric-indent-mode nil
 ;; Place all auto-save files in one directory.
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 ;; more useful frame title, that show either a file or a
 ;; buffer name (if the buffer isn't visiting a file)
 frame-title-format '((:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))
 ;; warn when opening files bigger than 100MB
 large-file-warning-threshold 100000000
 ;; Don't create backup files
 make-backup-files nil ; stop creating backup~ files
 ;; Remember my location when the file is last opened
 ;; activate it for all buffers
 save-place-file (expand-file-name "saveplace" user-emacs-directory)
 save-place t
 ;; smooth scrolling
 scroll-conservatively 101
 ;; turn off the bell
 ring-bell-function 'ignore
 ;; Smoother scrolling
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ;; scroll window under mouse
 scroll-step 1 ;; keyboard scroll one line at a time
 )

;; Misc
(set-frame-name "emacs")
(fringe-mode '(1 . 3))
(delete-selection-mode 1)
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; Set paste system
(set-clipboard-coding-system 'utf-16le-dos)
;; Set paste error under linux
(set-selection-coding-system 'utf-8)
;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)
;; Don't blink
(blink-cursor-mode 0)
;; Auto save visited file.
(auto-save-visited-mode 1)

;; ESC is mapped as metakey by default, very counter-intuitive.
;; Map escape to cancel (like C-g) everywhere
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(define-key isearch-mode-map "\e" 'isearch-abort)   ;; \e seems to work better for terminals
(global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else

;; Don't add custom section directly under init.el.
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; Marking and popping
(setq-default
 ;; Marking and popping
 set-mark-command-repeat-pop t
 ;; Real emacs knights don't use shift to mark things
 shift-select-mode nil)
;; When popping the mark, continue popping until the cursor actually
;; moves Also, if the last command was a copy - skip past all the
;; expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Early unbind keys for customization
(unbind-key "C-s") ; Reserve for search related commands
(unbind-key "C-z") ;; Reserve for hydra related commands

;; Quick access to commonly used files
(global-set-key (kbd "s-SPC") (lambda () (interactive) (find-file (expand-file-name ".emacs.d/init.el" my-emacs-conf-directory))))
(global-set-key (kbd "s-f") (lambda () (interactive) (find-file-other-window org-my-beancount-file)))

(use-package beacon
  ;; Highlight the cursor whenever it scrolls
  :defer 5
  :bind (("C-<f12>" . beacon-blink)) ;; useful when multiple windows
  :config
  (setq beacon-size 5)
  (beacon-mode 1))

(use-package uniquify
  ;; unique buffer names dependent on file name
  :straight nil
  :defer 5
  :config
  (setq
   ;; Rename buffers with same name
   uniquify-buffer-name-style 'forward
   uniquify-separator "/"
   ;; rename after killing uniquified
   uniquify-after-kill-buffer-p t
   ;; don't muck with special buffers
   uniquify-ignore-buffers-re "^\\*"))

(use-package hungry-delete
  :defer 3
  :config
  (global-hungry-delete-mode))

(use-package autorevert
  ;; revert buffers when files on disk change
  :defer 3
  :config
  (setq
   ;; Also auto refresh dired, but be quiet about it
   global-auto-revert-non-file-buffers t
   auto-revert-verbose nil
   ;; Revert pdf without asking
   revert-without-query '("\\.pdf"))
  (global-auto-revert-mode 1) ;; work with auto-save with Org files in Dropbox
  )

(use-package recentf
  :defer 5
  :config
  (setq recentf-save-file (expand-file-name "recentf" my-private-conf-directory)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  ;; save it every 5 minutes
  (run-at-time t (* 5 60) 'recentf-save-list)
  ;; Suppress output "Wrote /home/yiufung/.emacs.d/recentf"
  (advice-add 'recentf-save-list :around #'suppress-messages)
  (recentf-mode +1)
  )

(use-package aggressive-indent
  ;; Aggresive indent mode
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  )

(use-package ibuffer
  ;; Better buffer management
  :defer 3
  :straight ibuffer-tramp
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-o"     . nil)) ;; unbind ibuffer-visit-buffer-1-window
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-tramp-set-filter-groups-by-tramp-connection)
              (ibuffer-do-sort-by-alphabetic)))
  )

(use-package which-key
  :defer 3
  :config
  (which-key-mode)
  )

(use-package whole-line-or-region
  ;; If no region is active, C-w M-w will act on current line
  :defer 5
  ;; Right click to paste: I don't use the popup menu a lot.
  :bind ("<mouse-3>" . whole-line-or-region-yank)
  :config
  (whole-line-or-region-global-mode)
  )

(use-package crux
  ;; A handful of useful functions
  :defer 1
  :bind (
         ("C-x t"         . 'crux-swap-windows)
         ("C-c b"         . 'crux-create-scratch-buffer)
         ("C-x o"         . 'crux-open-with)
         ("C-x f"         . 'crux-recentf-find-file)
         ("C-x 4 t"       . 'crux-transpose-windows)
         ("C-x C-k"       . 'crux-delete-buffer-and-file)
         ("C-c n"         . 'crux-cleanup-buffer-or-region)
         ("s-<return>"    . 'crux-cleanup-buffer-or-region)
         ("C-<backspace>" . 'crux-kill-line-backwards)
         )
  :init
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  :config
  ;; Retain indentation in these modes.
  (add-to-list 'crux-indent-sensitive-modes 'markdown-mode)
  )

(use-package direnv
  :if (equal system-type 'gnu/linux)
  ;; Environment switcher to enable project-base environment variables
  ;; Checks for .envrc file and apply environment variable
  :defer 3
  :straight flycheck
  :straight t
  :config
  (direnv-mode)
  (eval-after-load 'flycheck
    '(setq flycheck-executable-find
           (lambda (cmd)
             (direnv-update-environment default-directory)
             (executable-find cmd))))
  )

(use-package simple
  ;; Improvements over simple editing commands
  :straight nil
  :defer 5
  :hook ((prog-mode markdown-mode) . auto-fill-mode)
  :bind (("<f8>" . (lambda () (interactive) (progn (visual-line-mode)
                                                   (follow-mode))))
         ;; M-backspace to backward-delete-word
         ("M-S-<backspace>" . backward-kill-sentence)
         ("M-C-<backspace>" . backward-kill-paragraph)
         ("C-x C-o"         . remove-extra-blank-lines)
         )
  :init
  ;; Move more quickly
  (global-set-key (kbd "C-S-n")
                  (lambda ()
                    (interactive)
                    (ignore-errors (next-line 5))))
  (global-set-key (kbd "C-S-p")
                  (lambda ()
                    (interactive)
                    (ignore-errors (previous-line 5))))
  ;; Show line num temporarily
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (linum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (linum-mode -1)))
  (global-set-key [remap goto-line] 'goto-line-with-feedback)

  (defun kill-region-or-backward-word ()
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))
  ;; (global-set-key (kbd "M-h") 'kill-region-or-backward-word)

  (defun remove-extra-blank-lines (beginning end)
    "If called with region active, replace multiple blank lines
with a single one.

Otherwise, call `delete-blank-lines'."
    (interactive "r")
    (if (use-region-p)
        (save-excursion
          (goto-char beginning)
          (while (re-search-forward "^\\([[:blank:]]*\n\\)\\{2,\\}" end t)
            (replace-match "\n")
            (forward-char 1)))
      (delete-blank-lines))
    )

  (defun alert-countdown ()
    "Show a message after timer expires. Based on run-at-time and can understand time like it can."
    (interactive)
    (let* ((msg-to-show (read-string "Message to show: "))
           (time-duration (read-string "Time: ")))
      (message time-duration)
      (run-at-time time-duration nil #'alert msg-to-show)))
  )

;;;; Misc defuns

;; Useful functions used by various packages, but not fit to put in anyone.

(defun suppress-messages (func &rest args)
  "Suppress message output from FUNC."
  ;; Some packages are too noisy.
  ;; https://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

(defun the-the ()
  ;; https://www.gnu.org/software/emacs/manual/html_node/eintr/the_002dthe.html
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

(defun sudo-shell-command (command)
  "Run COMMAND as root."
  (interactive "MShell command (root): ")
  (with-temp-buffer
    (cd "/sudo::/")
    (async-shell-command command)))

;;; Region Manipulation

;; Beautiful command from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(use-package expand-region
  ;; Incrementally select a region
  :after org ;; When using straight, er should byte-compiled with the latest Org
  :bind (("C-'" . er/expand-region)
         ("C-M-'" . er/contract-region))
  :config
  (defun org-table-mark-field ()
    "Mark the current table field."
    (interactive)
    ;; Do not try to jump to the beginning of field if the point is already there
    (when (not (looking-back "|[[:blank:]]?"))
      (org-table-beginning-of-field 1))
    (set-mark-command nil)
    (org-table-end-of-field 1))

  (defun er/add-org-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(org-table-mark-field))))

  (add-hook 'org-mode-hook 'er/add-org-mode-expansions)

  (setq expand-region-fast-keys-enabled nil
        er--show-expansion-message t))

(use-package wrap-region
  ;; Wrap selected region
  :hook ((prog-mode text-mode) . wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(
     ("$" "$")
     ("*" "*")
     ("=" "=")
     ("`" "`")
     ("/" "/")
     ("_" "_")
     ("~" "~")
     ("+" "+")
     ("/* " " */" "#" (java-mode javascript-mode css-mode))))
  (add-to-list 'wrap-region-except-modes 'ibuffer-mode)
  (add-to-list 'wrap-region-except-modes 'magit-mode)
  (add-to-list 'wrap-region-except-modes 'magit-todo-mode)
  (add-to-list 'wrap-region-except-modes 'magit-popup-mode)
  )

(use-package change-inner
  :bind (("M-I" . copy-inner)
         ("M-O" . copy-outer)
         ("s-i" . change-inner)
         ("s-o" . change-outer))
  )

;;; Text Editing / Substitution / Copy-Pasting

(use-package multiple-cursors
  ;; Read https://github.com/magnars/multiple-cursors.el for common use cases
  :defer 10
  :commands (mc/mark-next-like-this)
  :bind
  (
   ;; Common use case: er/expand-region, then add curors.
   ("C-}" . mc/mark-next-like-this)
   ("C-{" . mc/mark-previous-like-this)
   ;; After selecting all, we may end up with cursors outside of view
   ;; Use C-' to hide/show unselected lines.
   ("C-*" . mc/mark-all-like-this)
   ;; HOLLLY>>>> Praise Magnars.
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ;; highlighting symbols only
   ("C->" . mc/mark-next-word-like-this)
   ("C-<" . mc/mark-previous-word-like-this)
   ("C-M-*" . mc/mark-all-words-like-this)
   ;; Region edit.
   ("C-S-c C-S-c" . mc/edit-lines)
   )
  :config
  (define-key mc/keymap (kbd "<return>") nil)
  (setq mc/list-file (expand-file-name "mc-list.el" my-private-conf-directory))
  )

(use-package iedit
  ;; C-; to start iedit
  :defer 3)

(use-package visual-regexp
  :defer 5
  :bind (("C-c r" . 'vr/replace)
         ("C-c %"   . 'vr/query-replace))
  :config
  (global-set-key [remap query-replace] 'vr/query-replace) ;; M-%
  )

(use-package undo-propose
  :bind (("C-c u" . undo-propose))
  )

(use-package unfill
  :bind (("M-Q" . unfill-paragraph))
  :commands (unfill-paragraph unfill-region unfill-toggle)
  )

;;; Completion Framework: Ivy / Swiper / Counsel
(use-package counsel
  ;; specifying counsel will bring ivy and swiper as dependencies
  :demand t
  :straight ivy-hydra
  :straight ivy-rich
  :straight counsel-projectile
  :straight smex
  :bind (("M-s"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("C-h V"   . counsel-set-variable)
         ("C-h l"   . counsel-find-library)
         ("C-h u"   . counsel-unicode-char)
         ("C-c j"   . counsel-git-grep)
         ("C-s p"   . counsel-git-grep)
         ("C-c i"   . counsel-imenu)
         ("C-x l"   . counsel-locate)
         ("C-x C-r" . counsel-recentf)
         ;; Search-replace with ag and rg:
         ;; C-u prefix to choose search directory
         ;; C-c C-o opens an editable buffer
         ("C-s a"   . counsel-ag)
         ;; ("C-s R"   . counsel-rg)
         ("C-s r"   . counsel-rg)
         ("C-s f"   . counsel-file-jump) ;; Jump to a file below the current directory.
         ("C-s j"   . counsel-dired-jump);; Jump to directory under current directory
         )
  :config
  (ivy-mode 1)
  (ivy-rich-mode 1)
  (counsel-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (counsel-projectile-mode 1)
  (setq smex-save-file (expand-file-name "smex-items" my-private-conf-directory))
  (setq ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers t ;; show recent files as buffers in C-x b
        ivy-use-selectable-prompt t ;; C-M-j to rename similar filenames
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-count-format "(%d/%d) "
        ;; Useful settings for long action lists
        ;; See https://github.com/tmalsburg/helm-bibtex/issues/275#issuecomment-452572909
        max-mini-window-height 0.30)

  ;; Offer to create parent directories if they do not exist
  ;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
  (defun my-create-non-existent-directory ()
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t))))
  (add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

  ;; Kill virtual buffer too
  ;; https://emacs.stackexchange.com/questions/36836/how-to-remove-files-from-recentf-ivy-virtual-buffers
  (defun my-ivy-kill-buffer (buf)
    (interactive)
    (if (get-buffer buf)
        (kill-buffer buf)
      (setq recentf-list (delete (cdr (assoc buf ivy--virtual-buffers)) recentf-list))))

  (ivy-set-actions 'ivy-switch-buffer
                   '(("k" (lambda (x)
                            (my-ivy-kill-buffer x)
                            (ivy--reset-state ivy-last))  "kill")
                     ("j" switch-to-buffer-other-window "other window")
                     ("x" browse-file-directory "open externally")
                     ))

  (ivy-set-actions 'counsel-find-file
                   '(("j" find-file-other-window "other window")
                     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
                     ("x" counsel-find-file-extern "open externally")
                     ("k" delete-file "delete")
                     ("g" magit-status-internal "magit status")
                     ("r" counsel-find-file-as-root "open as root")))
  )

;;; File Nav & Mgmt: Follow / Dired / Bookmark+

(use-package follow
  :bind (("<f7>" . follow-delete-other-windows-and-split)
         ("<f8>" . follow-mode))
  )

(use-package dired
  :defer 3
  :straight async
  :bind ("C-x C-d" . dired) ;; Original list-directory is not useful.
  :config
  (require 'dired-x) ;; extra functionality for dired
  (setq dired-listing-switches "-alh"
        dired-dwim-target t
        dired-no-confirm '(copy))
  (dired-async-mode 1))

(use-package bookmark+
  ;; Bookmark utilities
  :straight t
  :after bookmark
  :defer 5
  :commands bmkp-jump-dired
  :init
  ;; Save bookmarks on every change
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "bookmarks" my-private-conf-directory))
  )

(use-package deft
  ;; :bind ("<f7>" . deft)
  :commands (deft)
  :config
  (setq deft-directory "~/Dropbox/journals/"
        deft-extensions '("md" "org")
        deft-recursive t
        )
  )

;;; Window and Buffer management
(use-package window
  ;; Handier movement over default window.el
  :straight nil
  :bind (
         ("C-x 2"             . split-window-below-and-move-there)
         ("C-x 3"             . split-window-right-and-move-there)
         ("C-x \\"            . toggle-window-split)
         ("C-0"               . delete-window)
         ("C-1"               . delete-other-windows)
         ("C-2"               . split-window-below-and-move-there)
         ("C-3"               . split-window-right-and-move-there)
         ("M-o"               . 'other-window)
         ("M-O"               . (lambda () (interactive) (other-window -1))) ;; Cycle backward
         ("M-<tab>"           . 'other-frame)
         ("<M-S-iso-lefttab>" . (lambda () (interactive) (other-frame -1))) ;; Cycle backwards
         )
  :init
  ;; Functions for easier navigation
  (defun split-window-below-and-move-there ()
    (interactive)
    (split-window-below)
    (windmove-down))

  (defun split-window-right-and-move-there ()
    (interactive)
    (split-window-right)
    (windmove-right))

  (defun toggle-window-split ()
    "When there are two windows, toggle between vertical and
horizontal mode."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  )

(use-package ace-window
  :defer 3
  :bind (("<C-return>" . ace-window)
         ("M-o"        . other-window)))

(use-package winner
  ;; Enable window restoration
  :defer 1
  :config
  (winner-mode 1))

(use-package shackle
  :defer 5
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-rules '((help-mode               :align below :select t)
                        (compilation-mode        :select t    :size 0.25)
                        ("*compilation*"         :select t    :size 0.25 :inhibit-window-quit t)
                        ("*ag search*"           :select nil  :size 0.25)
                        ("*Flycheck errors*"     :select nil  :size 0.25)
                        ("*Warnings*"            :select nil  :size 0.25)
                        ("*Error*"               :select nil  :size 0.25)
                        ("*Org Links*"           :select nil  :size 0.2)
                        ("^\\*git-gutter.+\\*$"  :regexp t    :size 15 :noselect t)
                        ;; Do not show Async Shell Command output unless explicitly asked for.
                        ("*Async Shell Command*" :ignore t)
                        ))
  :config
  (shackle-mode 1))

(use-package nswbuff
  ;; Quickly switching buffers. Quite useful!
  :bind (("<C-tab>"           . nswbuff-switch-to-next-buffer)
         ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
  :config
  (setq nswbuff-display-intermediate-buffers t)
  )

;;; Cursor navigation

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(use-package avy
  :bind  (("C-,"   . avy-goto-char-2)
          ("C-M-," . avy-goto-line))
  :commands (avy-with)
  :config
  (setq avy-timeout-seconds 0.3
        avy-all-windows 'all-frames
        avy-style 'at-full)
  )

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;;; Version-control: Magit

(use-package magit
  :defer 10
  :straight gitignore-templates
  :straight diff-hl
  :straight git-timemachine
  ;;display flycheck errors only on added/modified lines
  :straight magit-todos
  :straight ediff
  :init
  (straight-use-package '(magit-diff-flycheck :host github :repo "ragone/magit-diff-flycheck"))
  :bind (:map vc-prefix-map
              ("s" . 'git-gutter:stage-hunk)
              ("c" . 'magit-clone))
  :bind (("C-x v r" . 'diff-hl-revert-hunk)
         ("C-x v n" . 'diff-hl-next-hunk)
         ("C-x v p" . 'diff-hl-previous-hunk))
  :bind (("C-x M-g" . 'magit-dispatch-popup)
         ("C-x g" . magit-status)
         ("C-x G" . magit-dispatch))
  :config
  ;; Enable magit-file-mode, to enable operations that touches a file, such as log, blame
  (global-magit-file-mode)

  ;; Prettier looks, and provides dired diffs
  (use-package diff-hl
    :defer 3
    :commands (diff-hl-mode diff-hl-dired-mode)
    :hook (magit-post-refresh . diff-hl-magit-post-refresh)
    :hook (dired-mode . diff-hl-dired-mode)
    )

  ;; Provides stage hunk at buffer, more useful
  (use-package git-gutter
    :defer 3
    :commands (git-gutter:stage-hunk)
    :bind (:map vc-prefix-map
                ("s" . 'git-gutter:stage-hunk))
    )

  ;; Someone says this will make magit on Windows faster.
  (setq w32-pipe-read-delay 0)

  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes))
  (set-default 'magit-diff-refine-hunk t)
  ;; change default display behavior
  (setq magit-completing-read-function 'ivy-completing-read
        magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1
        magit-clone-set-remote.pushDefault nil
        magit-clone-default-directory "~/projects/")

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  ;; Set magit password authentication source to auth-source
  (add-to-list 'magit-process-find-password-functions
               'magit-process-password-auth-source)

  ;; Useful functions copied from
  ;; https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750
  ;; Combined with ~ to swap the order of the buffers you can get A then B or B then A
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

  ;; Always expand file in ediff
  (add-hook 'ediff-prepare-buffer-hook #'show-all)
  ;; Do everything in one frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  )

;;; Projectile + eyebrowse

(use-package projectile
  :defer 5
  :straight ripgrep ;; required by projectile-ripgrep
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (("C-c o" . 'projectile-find-file))
  :config
  ;; Where my projects and clones are normally placed.
  (setq projectile-project-search-path '("~/projects")
        projectile-completion-system 'ivy)
  (projectile-mode +1)
  )

(use-package eyebrowse
  :defer 2
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c w")) ;; w for workspace
  :bind
  (
   ("<f9>"      . 'eyebrowse-last-window-config)
   ("<f10>"     . 'eyebrowse-prev-window-config)
   ("<f11>"     . 'eyebrowse-switch-to-window-config)
   ("<f12>"     . 'eyebrowse-next-window-config)
   ("C-c w s"   . 'eyebrowse-switch-to-window-config)
   ("C-c w w"   . 'eyebrowse-switch-to-window-config)
   ("C-c w TAB" . 'eyebrowse-last-window-config)
   ("C-c w k"   . 'eyebrowse-close-window-config)
   ("C-c w n"   . 'eyebrowse-next-window-config)
   ("C-c w p"   . 'eyebrowse-prev-window-config))
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-close-window-config-prompt t
        eyebrowse-mode-line-style 'current
        eyebrowse-tagged-slot-format "%t")
  (eyebrowse-mode)
  )

;;; Org

(use-package org
  ;; Combining demand and org-plus-contrib to ensure the latest version of org is used
  :demand t
  :straight ob-ipython
  :straight ob-async
  :straight ob-http
  :straight org-bullets
  :straight org-super-agenda
  :straight org-pomodoro
  :straight org-present
  :straight org-chef
  :straight ox-gfm
  :straight org-download
  :straight ox-hugo
  :straight easy-hugo
  :straight gnuplot
  :straight helm-org-rifle
  :hook (org-mode . auto-fill-mode)
  :hook (org-mode . org-bullets-mode)
  :init
  ;; customized export formats
  (straight-use-package '(ox-ipynb :host github :repo "jkitchin/ox-ipynb"))
  :bind (
         ("C-c a" . org-agenda)
         ("C-c c" . counsel-org-capture)
         ("C-c l" . org-store-link)
         ("C-c 0" . org-set-created-property)
         ;; Rifle through all my org files to identify an item.
         ;; Use C-s to display results in occur-like style.
         ("C-S-s" . helm-org-rifle)
         ("s-e" . ivy-insert-org-entity)
         ("H-p" . org-pomodoro)
         :map org-mode-map
         ("C-c C-j" . counsel-org-goto)
         )
  :bind (:map org-mode-map
              ;; Unbinding org-cycle-agenda-files
              ("C-'"          . nil)
              ("C-,"          . nil)
              ;; Unbinding org-force-cycle-archived
              ("<C-tab>"      . nil)
              ;; default option respects content, I don't use it
              ("C-<return>"   . nil) ;; Reserved for ace-window
              ("C-S-<return>" . org-insert-heading-respect-content)
              ("s-n"          . org-next-block)
              ("s-p"          . org-previous-block)
              )
  :config
  ;; All org directory under Dropbox
  (setq org-directory (expand-file-name "journals" my-sync-directory))
  ;; Setup diary too
  (setq diary-file (expand-file-name "diary" org-directory))

  ;; Default org-mode startup
  (setq org-startup-folded t
        org-startup-with-inline-images t
        org-startup-with-latex-preview t)
  ;; Larger latex fragments
  (plist-put org-format-latex-options :scale 1.5)

  ;; set todo keywords. As of v9.2.3, any file-local keyword list will overwrite (instead of append) value set in here.
  ;; So actual tags used in Org files are specified using #+SEQ_TODO and #+TYP_TODO instead. Here I keep a complete
  ;; list of tags for font settings
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n)" "IN-PROGRESS(i!)" "WAIT(w@)" "BLOCKED(b@/!)" "SOMEDAY(s)" "CANCELLED(c@/!)" "DONE(d!)")))
  ;; Setup for ordered tasks. Initiate with C-c C-x o
  (setq org-enforce-todo-dependencies t)
  ;; If it's cancel, set ARCHIVE to be true, so that org-agenda won't show it
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("ARCHIVE" . t))
          ("TODO" ("ARCHIVE" . nil))
          ("NEXT" ("ARCHIVE" . nil))
          ("IN-PROGRESS" ("ARCHIVE" . nil))
          ("WAIT" ("ARCHIVE" . nil))
          ("BLOCKED" ("ARCHIVE" . nil))
          ("SOMEDAY" ("ARCHIVE" . nil))
          ("DONE" ("ARCHIVE" . nil)))
        )
  ;; Org-agenda
  (setq
   ;; All files for agenda
   org-agenda-files (list org-directory
                          (expand-file-name "notes" org-directory)
                          (expand-file-name "projects" org-directory)
                          (expand-file-name "orgzly" org-directory)
                          (expand-file-name "church" org-directory))
   ;; Refile candidates
   org-refile-targets '((org-agenda-files :maxlevel . 2))
   ;; Show candidates in one go
   org-outline-path-complete-in-steps nil
   ;; Show full paths for refiling
   org-refile-use-outline-path t
   ;; Use current window
   org-agenda-window-setup 'reorganize-frame
   org-agenda-restore-windows-after-quit t
   ;; It doesn't have to start on weekday
   org-agenda-start-on-weekday nil
   ;; Agenda view start on today
   org-agenda-start-day nil
   ;; Warn me in 2 weeks
   org-deadline-warning-days 14
   ;; Show state changes in org-agenda-view when log-mode is enabled. Press l.
   org-agenda-log-mode-items '(closed clock state)
   ;; Customized agenda-view
   org-agenda-custom-commands
   '(("x" agenda)
     ("y" agenda*)
     ("o" "Office"
      ((agenda "Hello" ((org-agenda-files (list
                                           org-my-office-file
                                           org-default-notes-file
                                           (expand-file-name "projects" org-directory)
                                           ))
                        ;; Show agenda for this whole week, and first 2 days of next week
                        (org-agenda-start-on-weekday 1) ;; Always start on Monday
                        (org-agenda-span 9)
                        ))))
     ;; ("o" "Office-related Agenda"
     ;;  ((agenda "" ((org-agenda-start-day "0d")
     ;;               (org-agenda-start-on-weekday nil)
     ;;               (org-agenda-span 1)
     ;;               (tags "@office")))))
     ("h" "Home"
      ((agenda "" ((org-agenda-files (list
                                      org-my-life-file
                                      org-my-plan-free-file
                                      org-my-web-archive-file
                                      (expand-file-name "bible.org" org-directory)
                                      (expand-file-name "church" org-directory)
                                      ))
                   (org-agenda-span 3) ;; Show upcoming 3 days
                   ))))
     ("c" "Church"
      ((agenda "" ((org-agenda-files (list (expand-file-name "church" org-directory)))))))
     )
   ;; Make it sticky, so it doesn't get killed upon hitting "q". Use "r" to
   ;; refresh instead. Note that it can still be killed by kill-buffer. To
   ;; avoid this, set the emacs-lock-mode
   org-agenda-sticky t
   ;; Don’t show scheduled items in agenda when they are done
   org-agenda-skip-scheduled-if-done t)

  ;; Auto save org-files, so that we prevent the locking problem between computers
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  ;; Suppress output "Saving all Org buffers... done"
  (advice-add 'org-save-all-org-buffers :around #'suppress-messages)

  ;; Tags
  (setq org-tag-alist '((:startgroup    . nil)
                        ("@home"        . ?h)
                        ("@office"      . ?o)
                        (:endgroup      . nil)
                        ("@project"     . ?p)
                        ("@appointment" . ?a)
                        ("@study"       . ?s)
                        ("@sport"       . ?S)
                        ("@finance"     . ?f)
                        ("@game"        . ?g)
                        ("@emacs"       . ?e)))
  ;; Capturing thoughts and Level 1 Headings.
  (setq org-default-notes-file (expand-file-name "plan-office.org" org-directory))
  (setq org-my-plan-free-file (expand-file-name "plan-free.org" org-directory))
  (setq org-my-plan-church-file (expand-file-name "church/plan-church.org" org-directory))
  (setq org-my-office-file (expand-file-name "office.org" org-directory))
  (setq org-my-web-archive-file (expand-file-name "web-archive.org" org-directory))
  (setq org-my-life-file (expand-file-name "life.org" org-directory))
  (setq org-my-beancount-file (expand-file-name "finance.beancount" org-directory))
  (setq org-capture-templates
        '(
          ("c" "all todos" ;; Capture first, refile later
           entry
           (file+headline org-my-plan-free-file "Schedule")
           "* TODO %?\n")

          ("C" "church" ;; Church todos
           entry
           (file+headline org-my-plan-church-file "Schedule")
           "* TODO %?\n")

          ("o" "office tasks"
           entry
           (file+headline org-default-notes-file "Schedule")
           "* TODO %?\n")

          ("m" "meeting"
           entry
           (file+headline org-my-office-file
                          "Meeting")
           "* TODO %^{prompt}\nSCHEDULED: %^t\n:PEOPLE:\n- %?\n:END:\n** Agenda\n** Action Items\n- [ ] "
           :prepend t)

          ("j" "working journal"
           plain
           (file+olp+datetree org-my-office-file "Working Journal")
           "%?\n"
           :prepend t)

          ("b" "finance book-keeping"
           plain
           (file+headline org-my-beancount-file "Expenses")
           "bc%?"  ;; yasnippet template
           :prepend t)

          ("d" "diary"
           plain
           (file+olp+datetree org-my-life-file "Diary")
           "%?\n")
          ))

  ;; Automatically add "CREATED" timestamp to org-capture entries
  ;; See https://emacs.stackexchange.com/questions/21291/add-created-timestamp-to-logbook
  ;; Change: Don't add property when filing at beancount files. It will create syntax error.
  (defvar org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")
  (defun org-set-created-property (&optional active NAME)
    "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified."
    (interactive)
    (let* ((created (or NAME org-created-property-name))
           (fmt (if active "<%s>" "[%s]"))
           (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
      (unless (or (org-entry-get (point) created nil)
                  ;; Beancount format does not accept :PROPERTY: syntax
                  (string-match "\\.beancount$" (buffer-name)))
        (org-set-property created now))))
  (add-hook 'org-capture-before-finalize-hook #'org-set-created-property)

  ;; General org settings
  (setq-default
   ;; Narrowing behavior
   org-indirect-buffer-display 'current-window
   ;; Insert Org-mode mode-line automatically on an empty line when `org-mode' is called
   org-insert-mode-line-in-empty-file t
   ;; Never leave empty lines in collapsed view, which makes headings more compact
   org-cycle-separator-lines 0
   ;; List demote sequence
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
   ;; List indent offsets, making it more apparent
   org-list-indent-offset 1
   ;; Increase imenu index depth
   org-imenu-depth 5
   ;; Logging settings: Better verbose than miss
   org-log-into-drawer t
   org-log-done 'note
   org-log-reschedule 'note
   org-log-redeadline 'note
   org-log-delschedule 'note
   org-log-deldeadline 'note
   ;; Setup log note templates. Add "to [new date]" in reschedule and redeadline
   org-log-note-headings '((done        . "CLOSING NOTE %t")
                           (state       . "State %-12s from %-12S %t")
                           (note        . "Note taken on %t")
                           (reschedule  . "Schedule changed on %t: %S -> %s")
                           (delschedule . "Not scheduled, was %S on %t")
                           (redeadline  . "Deadline changed on %t: %S -> %s")
                           (deldeadline . "Removed deadline, was %S on %t")
                           (refile      . "Refiled on %t")
                           (clock-out   . ""))
   ;; All entries in the subtree are considered todo items
   org-hierarchical-todo-statistics 'nil
   ;; Remove the markup characters, i.e., "/text/" becomes (italized) "text"
   org-hide-emphasis-markers t
   ;; resepect heading.
   org-insert-heading-respect-content nil
   ;; Warn when editing invisible area
   org-catch-invisible-edits 'show-and-error
   ;; Use C-c C-o to open links, but this should be handier.
   org-return-follows-link t
   )

  ;; Enable org-id for globally unique IDs
  (add-to-list 'org-modules 'org-id)
  (setq org-id-locations-file (expand-file-name ".org-id-locations" my-private-conf-directory)
        org-id-link-to-org-use-id 'create-if-interactive)

  ;; Enable org-habit
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (setq org-habit-show-all-today t
        org-habit-show-habits-only-for-today t
        org-habit-show-done-always-green t
        org-habit-graph-column 32
        org-habit-preceding-days 28
        org-habit-following-days 7)

  ;; Update cookie automatically
  (defun myorg-update-parent-cookie ()
    (when (equal major-mode 'org-mode)
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-update-parent-todo-statistics)))))
  (defadvice org-kill-line (after fix-cookies activate)
    (myorg-update-parent-cookie))
  (defadvice kill-whole-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

  ;; Enable link abbreviation
  (setq org-link-abbrev-alist '(("bugzilla"  . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
                                ("url-to-ja" . "http://translate.google.fr/translate?sl=en&tl=ja&u=%h")
                                ("google"    . "http://www.google.com/search?q=")
                                ("gmap"      . "http://maps.google.com/maps?q=%s")
                                ("omap"      . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
                                ("ads"       . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")
                                ("openrice"  . "https://www.openrice.com/en/hongkong/restaurants?what=%h")
                                ("jira"  . "https://asw-global-digital-transformation.atlassian.net/browse/%h")
                                ("youtube" . "https://www.youtube.com/results?search_query=%s")))

  (use-package org-my-html-export-style
    ;; My personal HTML export settings
    :no-require
    :straight ox-twbs
    :demand t
    :after org
    :init (require 'ox)
    :config
    ;; let Org/Htmlize assign classes only, and to use a style file to
    ;; define the look of these classes. See docs for more info.
    (setq-default org-html-htmlize-output-type 'css)

    ;; put your css files here
    ;; default css: http://sriramkswamy.github.io/dotemacs/org.css
    (setq org-theme-css-dir (expand-file-name "static/" my-emacs-conf-directory))

    ;; Use this function to allow exporting using css file. No need to define html_head
    (defun toggle-org-custom-inline-style ()
      (interactive)
      (let ((hook 'org-export-before-parsing-hook)
            (fun 'set-org-html-style))
        (if (memq fun (eval hook))
            (progn
              (remove-hook hook fun 'buffer-local)
              (message "Removed %s from %s" (symbol-name fun) (symbol-name hook)))
          (add-hook hook fun nil 'buffer-local)
          (message "Added %s to %s" (symbol-name fun) (symbol-name hook)))))
    ;; By default toggle to true
    (toggle-org-custom-inline-style)

    (defun org-theme ()
      (interactive)
      (let* ((cssdir org-theme-css-dir)
             (css-choices (directory-files cssdir nil ".css$"))
             (css (completing-read "theme: " css-choices nil t)))
        (concat cssdir css)))

    (defun set-org-html-style (&optional backend)
      (interactive)
      (when (or (null backend) (eq backend 'html))
        (let ((f (or (and (boundp 'org-theme-css) org-theme-css) (org-theme))))
          (if (file-exists-p f)
              (progn
                (set (make-local-variable 'org-theme-css) f)
                (set (make-local-variable 'org-html-head)
                     (with-temp-buffer
                       (insert "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n")
                       (insert-file-contents f)
                       (goto-char (point-max))
                       (insert "\n/*]]>*/-->\n</style>\n")
                       (buffer-string)))
                (set (make-local-variable 'org-html-head-include-default-style)
                     nil)
                (message "Set custom style from %s" f))
            (message "Custom header file %s doesnt exist")))))
    )

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython    . t)
     (emacs-lisp . t)
     (R          . t)
     (haskell    . t)
     (python     .t)
     (org        .t)
     (dot        .t)
     (sql        .t)
     (http       . t)
     (latex      . t)
     (js         . t)
     (shell      . t)
     (plantuml   . t)
     (ditaa      . t) ;; turn ascii art into bitmap graphics.
     ))

  ;; Setup code block templates.
  ;; For Org-mode < 9.2
  (setq old-structure-template-alist
        '(("py" "#+BEGIN_SRC python :results output\n?\n#+END_SRC" "")
          ("ipy" "#+BEGIN_SRC ipython :results output\n?\n#+END_SRC" "")
          ("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "")
          ("hs" "#+BEGIN_SRC haskell\n?\n#+END_SRC" "")
          ("laeq" "#+BEGIN_LaTeX\n\\begin{equation} \\label{eq-sinh}\ny=\\sinh x\n\\end{equation}\n#+END_LaTeX" "")
          ("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC" "")
          ("r" "#+BEGIN_SRC R\n?\n#+END_SRC" "")
          ("js" "#+BEGIN_SRC js\n?\n#+END_SRC" "")
          ("http" "#+BEGIN_SRC http\n?\n#+END_SRC" "")
          ("ditaa" "#+BEGIN_SRC ditaa :file\n?\n#+END_SRC" "")
          ("dot" "#+BEGIN_SRC dot :file\n?\n#+END_SRC" "")
          ("rp" "#+BEGIN_SRC R :results output graphics :file \n?\n#+END_SRC" "")
          ("plantuml" "#+BEGIN_SRC plantuml :file\n?\n#+END_SRC" "")
          ("n" "#+NAME: ?")
          ("cap" "#+CAPTION: ?")))
  ;; For Org-mode >= 9.2
  (setq new-structure-template-alist
        '(("py"       . "src python :results output")
          ("ipy"      . "src ipython :results output")
          ("el"       . "src emacs-lisp")
          ("hs"       . "src haskell")
          ("laeq"     . "latex \n\\begin{equation} \\label{eq-sinh}\ny=\\sinh x\\end{equation}")
          ("sh"       . "src sh")
          ("r"        . "src R")
          ("js"       . "src js")
          ("http"     . "src http")
          ("ditaa"    . "src ditaa :file")
          ("dot"      . "src dot :file")
          ("rp"       . "src R :results output graphics :file ")
          ("plantuml" . "src plantuml :file")
          ))
  ;; Keyword expansion also changed in 9.2
  (setq my-tempo-keywords-alist
        '(("n" . "NAME")
          ("cap" . "CAPTION")))

  ;; In org-version >= 9.2, structure template is changed.
  ;; Below line allows me to keep using previous patterns.
  (when (not (version< (org-version) "9.2"))
    (require 'org-tempo))

  ;; Now set structures for both new and old.
  (if (version<  (org-version) "9.2")
      (dolist (ele old-structure-template-alist)
        (add-to-list 'org-structure-template-alist ele))
    (dolist (ele new-structure-template-alist)
      (add-to-list 'org-structure-template-alist ele))
    (dolist (ele my-tempo-keywords-alist)
      (add-to-list 'org-tempo-keywords-alist ele))
    )

  ;; Plotting with ditaa and plantuml
  (setq org-ditaa-jar-path (expand-file-name "bin/ditaa.jar" my-emacs-conf-directory))
  (setq org-plantuml-jar-path (expand-file-name "bin/plantuml.jar" my-emacs-conf-directory))
  ;; For R plotting.
  ;; Default template uses :results output as header arguments.
  ;; If we want to use ggplot2, which plots to org AND saves a file in
  ;; directory, we need to use :results output graphics.

  (setq org-src-fontify-natively t   ; Syntax highlight in #+BEGIN_SRC blocks
        org-src-tab-acts-natively t ; Why isn't this default?
        org-src-window-setup 'current-window ;; Edit source block location
        org-edit-src-content-indentation 0
        org-fontify-quote-and-verse-blocks t ;; fontify text within verse/quote
        org-adapt-indentation nil)

  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  ;; Fix an incompatibility between the ob-async and ob-ipython packages
  (setq ob-async-no-async-languages-alist '("ipython"))
  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images
            'append)

  ;; Org export to doc
  (setq org-odt-preferred-output-format "docx"
        org-odt-fontify-srcblocks t)

  ;; Org Speed commands
  (setq org-use-speed-commands t)
  (add-to-list 'org-speed-commands-user (cons "P" 'org-set-property))
  (add-to-list 'org-speed-commands-user (cons "d" 'org-deadline))
  (add-to-list 'org-speed-commands-user (cons "S" 'org-schedule))
  ;; Use indirect buffer instead of narrowing, so that visibility of original
  ;; buffer is not changed.
  ;; Widen is replace as toggle too.
  (add-to-list 'org-speed-commands-user (cons "s" 'org-tree-to-indirect-buffer))
  ;; Mark a subtree
  (add-to-list 'org-speed-commands-user (cons "m" 'org-mark-subtree))
  ;; kill a subtree
  (add-to-list 'org-speed-commands-user (cons "k" (lambda ()
                                                    (org-mark-subtree)
                                                    (kill-region
                                                     (region-beginning)
                                                     (region-end)))))
  ;; Jump to headline
  (add-to-list 'org-speed-commands-user (cons "j" (lambda ()
                                                    (avy-with avy-goto-line
                                                      (avy--generic-jump "^\\*+" nil)))))

  ;; Capturing pages from web. Integrates org-protocol-capture-html,
  ;; org-capture-extensions and org-protocol
  (use-package org-protocol-capture-html
    :defer 3
    :straight (:host github :repo "alphapapa/org-protocol-capture-html")
    :straight org-web-tools
    :bind (:map org-mode-map
                ("C-c C-S-l" . org-web-tools-insert-link-for-url))
    :config
    ;; org-capture-extension
    ;; Suitable for marking a link, or selected text within a page
    (require 'org-protocol)
    (defun transform-square-brackets-to-round-ones(string-to-transform)
      "Transforms [ into ( and ] into ), other chars left unchanged."
      (concat
       (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
      )
    (add-to-list 'org-capture-templates
                 '("p" "Protocol" entry (file+headline org-my-web-archive-file "Archives")
                   "* %?[[%:link][%:description]]\n%i"))
    (add-to-list 'org-capture-templates
                 '("L" "Protocol Link" entry (file+headline org-my-web-archive-file "Archives")
                   "* %?[[%:link][%:description]]"))
    ;; org-procotol-capture-html
    ;; Turning the whole HTML as Org entry, using pandoc for formatting,
    ;; downloading all pics etc.
    ;; Useful for archiving.
    (add-to-list 'org-capture-templates
                 '("w" "Web site" entry
                   (file+olp org-my-web-archive-file "Archives")
                   "* %a %?\n%:initial"))

    (defun search-forward-and-org-download-images()
      "Search forward for HTTP Image URLs, replace each using
org-download-image to obtain a local copy."
      (interactive)
      (while (re-search-forward org-bracket-link-regexp nil t)
        (let* (
               (end (match-end 0))
               (beg (match-beginning 0))
               (s (buffer-substring-no-properties beg end))
               (match? (string-match org-bracket-link-regexp s))
               (link (match-string 1 s))
               )
          (when (string-match "^http.*?\\.\\(?:png\\|jpg\\|jpeg\\)\\(.*\\)$"
                              link) ;; This is an image link
            (message (concat "Downloading image: "link))
            (delete-region beg end)
            (org-download-image link)
            (sleep-for 1) ;; Some sites dislike frequent requests
            ))))
    )

  ;; ob-ipython
  (setq ob-ipython-resources-dir "/tmp/ob-ipython-resources/")

  ;; ox-hugo setup
  (with-eval-after-load 'ox
    (require 'ox-hugo))
  ;; easy-hugo setup
  (setq easy-hugo-basedir (expand-file-name "blog" my-sync-directory))

  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
          See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:"
                     "%?\n")          ;Place the cursor here finally
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("H"                ;`org-capture' binding + h
                   "Blog post (Hugo)"
                   entry
                   (file+olp "blog.org" "Blog Ideas")
                   (function org-hugo-new-subtree-post-capture-template))))

  ;; org-download
  (require 'org-download)
  ;; org-download use buffer-local variables. Set it individually in files. Otherwise, put things flatly in misc
  ;; folder.
  (setq-default org-download-image-dir
                (expand-file-name "images/misc" org-directory)
                org-download-heading-lvl nil
                org-download-screenshot-method "scrot -s %s"
                org-download-image-org-width 400
                ;; org-download-image-latex-width 5
                ;; org-download-image-html-width 400
                org-download-annotate-function (lambda (link) "") ;; Don't annotate
                )
  (global-set-key (kbd "C-c S") 'org-download-screenshot)
  ;; Use #+ATTR_ORG: :width 300px to customized image display width
  (setq org-image-actual-width nil)

  (use-package org-recent-headings
    :after counsel
    :config (org-recent-headings-mode))

  ;; org-chef
  (require 'org-chef)

  ;; org-contacts
  (use-package org-contacts
    :defer t
    :straight nil
    :after (org org-capture)
    :config
    (add-to-list 'org-capture-templates
                 '("lc"  "contact" entry
                   (file+headline "contacts.org" "To File")
                   "* %(org-contacts-template-name)
      :PROPERTIES:
      :EMAIL: %(org-contacts-template-email)
      :PHONE: %^{Phone}
      :ADDRESS: %^{Home Address}
      :BIRTHDAY: %^{yyyy-mm-dd}
      :ORG:  %^{Company}
      :NOTE: %^{NOTE}
      :END:"
                   :empty-lines 1))
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org-ref + Org-noter integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load org-ref, and use ivy for completion
  (use-package org-ref
    :defer 10
    :straight t
    :straight ivy-bibtex
    :straight org-noter
    :bind ("H-b" . ivy-bibtex) ;; open bibliography
    :init
    ;; Common variables
    (setq my-bibliography-directory (expand-file-name "bibliography" my-sync-directory))
    (setq my-default-bibliography (expand-file-name
                                   "references.bib" my-bibliography-directory))
    (setq my-default-notes (expand-file-name
                            "notes/paper-notes.org" org-directory))
    ;; variables for ivy-bibtex, which uses bibtex-completion
    (setq bibtex-completion-bibliography `(,my-default-bibliography)
          bibtex-completion-notes-path my-default-notes
          bibtex-completion-library-path `(,(expand-file-name "pdfs/" my-bibliography-directory)))
    ;; variables for org-ref-ivy
    (setq reftex-default-bibliography my-default-bibliography)
    ;; variables for org-ref
    (setq org-ref-bibliography-notes my-default-notes
          org-ref-default-bibliography `(,my-default-bibliography)
          org-ref-pdf-directory (expand-file-name
                                 "pdfs/" ;; Must have / ending for working with org-noter
                                 my-bibliography-directory))
    ;; variables for org-noter
    (setq org-noter-notes-search-path `(,(expand-file-name "notes" org-directory))
          org-noter-default-notes-file-names '("paper-notes.org"))

    ;; Integrate org-ref + org-noter
    ;; Add NOTER_DOCUMENT to org-ref template
    (setq org-ref-note-title-format
          "** TODO %y - %t
 :PROPERTIES:
  :Custom_ID: %k
  :AUTHOR: %9a
  :NOTER_DOCUMENT: %F
  :JOURNAL: %j
  :YEAR: %y
  :VOLUME: %v
  :PAGES: %p
  :DOI: %D
  :URL: %U
 :END:
")

    :config
    ;; Load libraries
    (require 'org-ref-ivy)
    (require 'ivy-bibtex)
    (require 'org-noter)

    ;; Integrate org-ref + ivy-bibtex
    ;; Uses org-ref function to open notes
    ;; See https://github.com/jkitchin/org-ref/issues/225
    (defun my-ivy/org-ref-notes-function (keys)
      (dolist (key keys)
        (funcall org-ref-notes-function key)))
    (ivy-bibtex-ivify-action my-ivy/org-ref-notes-function ivy-bibtex-edit-notes)

    ;; Usage:
    ;; General entrance: org-ref
    ;; Use arxiv-get-pdf-add-bibtex-entry to download and add entry
    ;; Use C-c ] (org-ref-insert-link) to insert citation.
    ;; When point at citation, H-o opens the hydra. Useful to navigate to relative notes.
    ;; Use org-ref-insert-cite-with-completion for special formatting
    ;; For PDFs that don't have a DOI, add references in .bib and rename pdfs with correct key.
    ;; Google Scholar has a "cite" button that's convenient for this purpose
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org-ref + Org-noter integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (use-package ox-latex
    ;; Org-latex output global settings
    ;;
    ;; 1. Source code fontification
    ;; 2. Chinese output
    ;;
    ;; Dependencies:
    ;; pacman -S python-pygments pygmentize
    :straight nil
    :init
    ;; Below settings are accompanied with my own LaTeX options which are defined in a yasnippet.
    ;; Consult https://github.com/dfeich/org-babel-examples/blob/master/latex/latex-example.org for example.
    (setq-default
     ;; Use xelatex by default
     org-latex-compiler "xelatex"
     ;; Export in background
     org-export-in-background 'nil) ;; TODO: Seems buggy when set to t

    ;; Enable source code fontification
    (setq-default org-latex-listings 'minted)  ;; Use python minted to fontify
    (setq org-latex-minted-options '(("frame" "lines") ("fontsize" "\\footnotesize")))

    ;; PDF output process with comments
    ;; 1. `--shell-escape' required for minted. See `org-latex-listings'
    ;; 2. "bibtex %b" ensures bibliography (of org-ref) is processed during pdf generation
    ;; 3. Remove output logs, out, auto at the end of generation
    (setq org-latex-pdf-process
          '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
            ))

    )

  ;; Add beamer output support
  (require 'ox-beamer)

  ;; org-present
  (eval-after-load "org-present"
    '(progn
       (add-hook 'org-present-mode-hook
                 (lambda ()
                   (org-present-big)
                   (org-display-inline-images)
                   (org-present-hide-cursor)
                   (org-present-read-only)
                   (hide-mode-line-mode)))
       (add-hook 'org-present-mode-quit-hook
                 (lambda ()
                   (org-present-small)
                   (org-remove-inline-images)
                   (org-present-show-cursor)
                   (org-present-read-write)))))

  ;; org-super-agenda
  ;; Disabled for now. It seems rather distracting sometimes.
  ;; (setq org-super-agenda-groups
  ;;       '((:name "Schedule"
  ;;                :time-grid t)
  ;;         (:name "Due today"
  ;;                :deadline today)
  ;;         (:name "Today"
  ;;                :and (:scheduled today :not (:habit t))) ;; Show habits separately.
  ;;         (:name "Overdue"
  ;;                :deadline past)
  ;;         (:name "Habits"
  ;;                :habit t)
  ;;         (:name "Due soon"
  ;;                :deadline future)
  ;;         (:name "Scheduled earlier"
  ;;                :scheduled past)
  ;;         ))
  ;; (org-super-agenda-mode)

  ;; helm-org-rifle
  (setq helm-org-rifle-show-path t)

  ;; Export to Confluence Wiki
  (require 'ox-confluence)

  ;; Use org-bookmark-heading
  (use-package org-bookmark-heading
    :defer 3)
  )

(use-package outshine
  ;; Easier navigation for source code files
  :defer 3
  :bind (:map outshine-mode-map
              ("<S-iso-lefttab>" . outshine-cycle-buffer)
              ;; ("<backtab>" . outshine-cycle-buffer) ;; For Windows
              )
  :hook (emacs-lisp-mode . outshine-mode)
  :config
  (setq outshine-cycle-emulate-tab t)
  )

;;; Mail
;; My email chain
;; Mailbox synchronizer: mbsync/isync
;; Mailbox indexer: notmuch
;; Initial tagging for mails: afew
;; Reading email: notmuch-emacs
;; Sending email: msmtp/msmtp-mta

;; General mail settings
(setq-default
 ;; Sendmail is an alias to msmtp after installing msmtp and msmtp-mta
 message-send-mail-function 'message-send-mail-with-sendmail
 ;; Three variables to work with msmtp to enable multiple accounts
 mail-specify-envelope-from 't
 message-sendmail-envelope-from 'header
 mail-envelope-from 'header
 )

(use-package notmuch
  ;; Another mail client. It's better at email searching
  :defer 3
  :straight nil
  :bind (("C-c m" . 'notmuch)
         :map notmuch-hello-mode-map
         ("m" . nil)
         ("c" . notmuch-mua-new-mail)
         ("G" . nil)
         ("g" . notmuch-poll-and-refresh-this-buffer)
         ("/" . notmuch-search)
         :map notmuch-search-mode-map
         ("G" . nil)
         ("g" . notmuch-poll-and-refresh-this-buffer)
         ("/" . notmuch-search)
         )
  :commands (notmuch)
  :init
  (setq-default
   notmuch-fcc-dirs 'nil  ;; Don't save sent emails in a separate folder
   notmuch-search-oldest-first 'nil
   notmuch-saved-searches '(
                            (:name "(u)nread" :query "tag:inbox AND tag:unread AND (NOT tag:subscription)" :key "u")
                            (:name "(l)ist-subscription" :query "tag:inbox AND tag:unread AND tag:subscription" :key "l")
                            (:name "(c)hurch" :query "tag:church" :key "c")
                            (:name "(f)astmail" :query "tag:fastmail" :key "f")
                            (:name "(g)mail" :query "tag:gmail" :key "g")
                            (:name "(w)ic_it" :query "tag:wic_it" :key "w")
                            (:name "(i)nbox" :query "tag:inbox" :key "i")
                            (:name "(e)macs-devel" :query "tag:lists/emacs-devel" :key "e")
                            (:name "(b)bdb-user" :query "tag:lists/bbdb-user" :key "b")
                            (:name "emacs-(o)rgmode" :query "tag:lists/emacs-orgmode" :key "o")
                            (:name "(n)as" :query "tag:nas" :key "n")
                            (:name "(s)ent" :query "tag:sent" :key "s")
                            (:name "(d)rafts" :query "tag:draft" :key "d")
                            (:name "(a)ll" :query "*" :key "a"))
   notmuch-always-prompt-for-sender t
   notmuch-show-empty-saved-searches t
   notmuch-archive-tags '("-inbox")
   notmuch-column-control t)
  :config
  (require 'notmuch) ;; Installed by pacman
  (defun notmuch-mark-as-read-and-move-next ()
    (interactive)
    (notmuch-search-tag
     (if (member "unread" (notmuch-search-get-tags))
         "-unread" "+unread"))
    )

  ;; Enable notmuch links in Org-mode
  (with-eval-after-load 'org
    (require 'ol-notmuch)
    )

  ;; Viewing diffs in notmuch. See https://notmuchmail.org/emacstips/#index25h2
  (defun my-notmuch-show-view-as-patch ()
    "View the the current message as a patch."
    (interactive)
    (let* ((id (notmuch-show-get-message-id))
           (msg (notmuch-show-get-message-properties))
           (part (notmuch-show-get-part-properties))
           (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
           (diff-default-read-only t)
           (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
           (map (make-sparse-keymap)))
      (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
      (switch-to-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert subject)
        (insert (notmuch-get-bodypart-text msg part nil)))
      (set-buffer-modified-p nil)
      (diff-mode)
      (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
        (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
      (goto-char (point-min))))
  (define-key 'notmuch-show-part-map "d" 'my-notmuch-show-view-as-patch)
  )

;;; Remote Editing: Tramp

(use-package tramp
  ;; Remote editing
  :defer 5
  :straight t
  :straight counsel-tramp
  :bind ("C-c t" . counsel-tramp)
  :config
  (setq tramp-default-method "ssh")
  ;; jww (2018-02-20): Without this change, tramp ends up sending hundreds of
  ;; shell commands to the remote side to ask what the temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name (expand-file-name "tramp" my-private-conf-directory))
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 1)
  )

;;; View Documents: DocView / PDF-Tools / Nov.el

(use-package doc-view
  ;; Requires unoconv, ghostscript, dvipdf
  :custom (doc-view-odf->pdf-converter-program "soffice")
  )

(use-package pdf-tools
  :defer t
  ;; :pin manual ;; manually update
  :straight tablist
  :straight hydra
  :load-path (lambda () (if (memq system-type '(windows-nt)) ;; If under Windows, use the customed build in Dropbox.
                            (expand-file-name "elisp/pdf-tools-20180428.827/"
                                              my-emacs-conf-directory)))
  ;; Tell Emacs to autoloads the package
  :init (load "pdf-tools-autoloads" nil t)
  ;; If under Linux, manually install it with package-install.
  ;; If there's error for pdf-occur mode, delete pdf-occur.elc manually.
  :bind (:map pdf-view-mode-map
              ("C-s" . 'isearch-forward)
              ("C-r" . 'isearch-backward)
              ("C-z p" . hydra-pdftools/body)
              ("M-w" . 'pdf-view-kill-ring-save)
              )
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-height)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotation t)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)

  (pdf-tools-install t) ;; Install pdf tools with no queries

  (defhydra hydra-pdftools (:color blue :hint nil)
    "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^      [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤     [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^      [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
    ("\\" hydra-master/body "back")
    ("<ESC>" nil "quit")
    ("al" pdf-annot-list-annotations)
    ("ad" pdf-annot-delete)
    ("aa" pdf-annot-attachment-dired)
    ("am" pdf-annot-add-markup-annotation)
    ("at" pdf-annot-add-text-annotation)
    ("y"  pdf-view-kill-ring-save)
    ("+" pdf-view-enlarge :color red)
    ("-" pdf-view-shrink :color red)
    ("0" pdf-view-scale-reset)
    ("H" pdf-view-fit-height-to-window)
    ("W" pdf-view-fit-width-to-window)
    ("P" pdf-view-fit-page-to-window)
    ("n" pdf-view-next-page-command :color red)
    ("p" pdf-view-previous-page-command :color red)
    ("d" pdf-view-dark-minor-mode)
    ("b" pdf-view-set-slice-from-bounding-box)
    ("r" pdf-view-reset-slice)
    ("g" pdf-view-first-page)
    ("G" pdf-view-last-page)
    ("e" pdf-view-goto-page)
    ("o" pdf-outline)
    ("s" pdf-occur)
    ("i" pdf-misc-display-metadata)
    ("u" pdf-view-revert-buffer)
    ("F" pdf-links-action-perfom)
    ("f" pdf-links-isearch-link)
    ("B" pdf-history-backward :color red)
    ("N" pdf-history-forward :color red)
    ("l" image-forward-hscroll :color red)
    ("h" image-backward-hscroll :color red))
  )

(use-package nov
  ;; epub support
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (add-to-list 'nov-mode-hook '(lambda () (beacon-mode -1)))
  )

;;; Spell-checking / Dictionary Lookup

(use-package flyspell
  :if (equal system-type 'gnu/linux)
  :straight t
  :straight flyspell-correct
  :straight flyspell-correct-ivy
  :straight auto-correct
  :hook ((text-mode gfm-mode markdown-mode org-mode) . flyspell-mode)
  :hook (prog-mode . flyspell-prog-mode)
  ;; any future corrections made with flyspell will be automatically corrected as I type.
  :hook (flyspell-mode . auto-correct-mode)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :bind (("H-s" . flyspell-buffer)
         :map flyspell-mode-map
         ("C-;" . nil)                  ; unbind the key, reserved for iedit
         ("C-," . nil)                  ; unbind the key, reserved for avy-jump
         ("C-." . flyspell-auto-correct-previous-word) ; Call with C-u to enable rapid mode.
         )
  :config
  ;; Requires aspell support.
  ;; Dictionaries to be downloaded via OS package manager
  (setq flyspell-default-dictionary "american"
        ispell-personal-dictionary (expand-file-name "ispell_dict"
                                                     my-private-conf-directory))
  ;; Suppress ispell output
  (advice-add 'ispell-change-dictionary :around #'suppress-messages)
  ;; only correct mistakes in a programming mode buffer that fall within a comment
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq auto-correct-predicate (lambda () (nth 8 (syntax-ppss (point)))))))
  ;; For text mode all the time
  (add-hook 'text-mode-hook
            (lambda ()
              (setq auto-correct-predicate (lambda () t))))
  ;; don't work in source blocks in Org mode
  (add-hook
   'org-mode-hook
   (lambda ()
     (setq auto-correct-predicate
           (lambda () (not (org-in-src-block-p))))))
  )

(use-package sdcv
  ;; Download from https://github.com/manateelazycat/sdcv (Not pluskid's sdcv-mode.el)
  ;; Dictionaries downloaded from http://download.huzheng.org/zh_CN/
  ;; See also https://wiki.archlinux.org/index.php/Sdcv
  :straight (:host github :repo "manateelazycat/sdcv"
                   :fork (:host github :repo "yiufung/sdcv"))
  :straight posframe
  :load-path (lambda () (expand-file-name "elisp" my-emacs-conf-directory))
  :commands (sdcv-search-pointer sdcv-search-pointer+ sdcv-search-input sdcv-search-input+)
  :custom-face (sdcv-tooltip-face ((t (:foreground "black" :background "gainsboro"))))
  :custom-face (internal-border ((t (:background "LightGrey"))))
  :bind (("C-c D" . sdcv-search-pointer)
         ("C-c d" . sdcv-search-pointer+))
  :bind (:map sdcv-mode-map
              ("s" . outline-show-entry)
              ("h" . outline-hide-entry)
              ("n" . sdcv-next-dictionary)
              ("p" . sdcv-previous-dictionary)
              ("l" . recenter-top-bottom)
              ("<tab>" . cyf-toggle-sdcv-entry)
              ("<S-iso-lefttab>" . cyf-toggle-sdcv-all) ;; <S-tab>
              )
  :hook (sdcv-mode . visual-line-mode)
  :config
  ;; Helper for toggling entry
  (defun cyf-toggle-sdcv-entry ()
    "Toggle entry for sdcv-mode."
    (interactive)
    (if (get 'cyf-toggle-sdcv-entry 'state)
        (progn
          (outline-hide-entry)
          (put 'cyf-toggle-sdcv-entry 'state nil))
      (progn
        (outline-show-entry)
        (put 'cyf-toggle-sdcv-entry 'state t))))
  ;; Helper for toggling all trees
  (defun cyf-toggle-sdcv-all ()
    "Toggle entry for sdcv-mode."
    (interactive)
    (if (get 'cyf-toggle-sdcv-all 'state)
        (progn
          (outline-hide-body)
          (put 'cyf-toggle-sdcv-all 'state nil))
      (progn
        (outline-show-all)
        (put 'cyf-toggle-sdcv-all 'state t))))
  ;; set dictionary path
  (setq sdcv-dictionary-simple-list '("懒虫简明英汉词典")
        sdcv-dictionary-complete-list '(
                                        "Webster's Revised Unabridged Dictionary (1913)"
                                        "牛津高阶英汉双解"
                                        "Oxford Advanced Learner's Dictionary"
                                        "牛津现代英汉双解词典"
                                        "懒虫简明英汉词典"
                                        "WordNet"
                                        )
        sdcv-tooltip-border-width 5
        sdcv-tooltip-timeout 5
        sdcv-dictionary-data-dir (expand-file-name "stardict" my-private-conf-directory))
  )

;;; Programming

;; General conventions on keybindings:
;; Use C-c C-z to switch to inferior process
;; Use C-c C-c to execute current paragraph of code

;;;; General settings: prog-mode, whitespaces, symbol-prettifying, highlighting
(use-package prog-mode
  ;; Generic major mode for programming
  :straight rainbow-delimiters
  :defer 5
  :hook (org-mode . prettify-symbols-mode)
  :hook (prog-mode . rainbow-delimiters-mode) ; Prettify parenthesis
  :hook (prog-mode . show-paren-mode)
  :init
  ;; Default to 80 fill-column
  (setq-default fill-column 80)
  ;; Prettify symbols
  (setq-default prettify-symbols-alist
                '(("#+BEGIN_SRC"     . "λ")
                  ("#+END_SRC"       . "λ")
                  ("#+RESULTS"       . ">")
                  ("#+BEGIN_EXAMPLE" . "¶")
                  ("#+END_EXAMPLE"   . "¶")
                  ("#+BEGIN_QUOTE"   . "『")
                  ("#+END_QUOTE"     . "』")
                  ("#+begin_src"     . "λ")
                  ("#+end_src"       . "λ")
                  ("#+results"       . ">")
                  ("#+begin_example" . "¶")
                  ("#+end_example"   . "¶")
                  ("#+begin_quote"   . "『")
                  ("#+end_quote"     . "』")
                  ))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (global-prettify-symbols-mode +1) ;; This only applies to prog-mode derived modes.
  )

(use-package editorconfig
  ;; Detect editorconfig setups
  :defer 3
  :config
  (editorconfig-mode 1))

(use-package smart-dash
  ;; Typing test-test gives test_that. Useful for naming variables.
  :hook (c-mode . smart-dash-mode)
  )

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode
             whitespace-turn-off)
  :preface
  (defvar normalize-hook nil)

  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (run-hook-with-args normalize-hook)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "depending on the file, maybe clean up whitespace."
    (when (and (not (or (memq major-mode '(markdown-mode))
                        (and buffer-file-name
                             (string-match "\\(\\.texi\\|COMMIT_EDITMSG\\)\\'"
                                           buffer-file-name))))
               (locate-dominating-file default-directory ".clean")
               (not (locate-dominating-file default-directory ".noclean")))
      (whitespace-mode 1)
      ;; For some reason, having these in settings.el gets ignored if
      ;; whitespace loads lazily.
      (setq whitespace-auto-cleanup t
            whitespace-line-column 80
            whitespace-rescan-timer-time nil
            whitespace-silent t
            whitespace-style '(face trailing lines space-before-tab empty))
      (add-hook 'write-contents-hooks
                #'(lambda () (ignore (whitespace-cleanup))) nil t)
      (whitespace-cleanup)))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)
  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer))

(use-package whitespace-cleanup-mode
  ;; Automatically cleanup whitespace
  :defer 3
  :config
  (global-whitespace-cleanup-mode))

(use-package hl-todo
  ;; Highlight all TODO keywords
  :defer 3
  :config
  (global-hl-todo-mode))

(use-package symbol-overlay
  ;; https://github.com/wolray/symbol-overlay
  :defer 3)

(use-package fill-column-indicator
  ;; Add visual indicator of fill-column
  ;; Some codes are here only to solve incompatibility issue with company-mode
  ;; https://github.com/company-mode/company-mode/issues/180#issuecomment-55047120
  :commands (fci-mode)
  ;; :after company
  ;; :hook (company-completion-started   . 'company-turn-off-fci)
  ;; :hook (company-completion-finished  . 'company-maybe-turn-on-fci)
  ;; :hook (company-completion-cancelled . 'company-maybe-turn-on-fci)
  :init
  ;; (defvar-local company-fci-mode-on-p nil)
  :config
  ;; (defun company-turn-off-fci (&rest ignore)
  ;;   (when (boundp 'fci-mode)
  ;;     (setq company-fci-mode-on-p fci-mode)
  ;;     (when fci-mode (fci-mode -1))))

  ;; (defun company-maybe-turn-on-fci (&rest ignore)
  ;;   (when company-fci-mode-on-p (fci-mode 1)))

  (setq fci-rule-color "#111122")
  )

;;;; Auto-completion with Company

(use-package company
  :defer 3
  :straight company-quickhelp ; Show short documentation at point
  :bind (
         :map company-active-map
         ("C-c ?" . company-quickhelp-manual-begin)
         )
  :config
  (setq company-show-numbers t
        ;; invert the navigation direction if the the completion
        ;; popup-isearch-match is displayed on top (happens near the bottom of
        ;; windows)
        company-tooltip-flip-when-above t)

  ;; Directly press [1..9] to insert candidates
  ;; See http://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (or (cl-find-if (lambda (s) (string-match re s))
                          company-candidates)
              (> (string-to-number k)
                 (length company-candidates)))
          (self-insert-command 1)
        (company-complete-number
         (if (equal k "0")
             10
           (string-to-number k))))))

  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
          (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))

  (global-company-mode t)
  )

;;;; Heuristic text completion: hippie expand + dabbrev
(use-package hippie-exp
  :straight nil
  :defer 3
  :bind (("M-/"   . hippie-expand-no-case-fold)
         ("C-M-/" . dabbrev-completion))
  :config
  ;; Don't case-fold when expanding with hippe
  (defun hippie-expand-no-case-fold ()
    (interactive)
    (let ((case-fold-search nil))
      (hippie-expand nil)))

  ;; hippie expand is dabbrev expand on steroids
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))

  )

(use-package abbrev
  :straight nil
  :defer 5
  :hook ((text-mode prog-mode erc-mode LaTeX-mode) . abbrev-mode)
  :init
  (setq save-abbrevs 'silently)
  :config
  (setq-default abbrev-file-name (expand-file-name "abbrev_defs" my-private-conf-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


;;;; Code folding

(use-package hideshow
  :bind (:map prog-mode-map
              ("C-c h" . hs-toggle-hiding)))

(use-package origami
  ;; Code folding
  :defer 3
  :after hydra
  :bind(
        ;; ("C-c f" . 'origami-toggle-node)
        ("C-z o" . hydra-origami/body)
        )
  :config
  (global-origami-mode)
  (defhydra hydra-origami (:color red)
    "
        _o_pen node    _n_ext fold       toggle _f_orward
        _c_lose node   _p_revious fold   toggle _a_ll
        "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes))
  )

;;;; Documentation

(use-package eldoc
  ;; Show argument list of function call at echo area
  :hook ((c-mode-common
          emacs-lisp-mode
          lisp-interaction-mode
          eval-expression-minibuffer-setup
          ielm-mode) . eldoc-mode)
  )

;;;; Source code navigation

(use-package dumb-jump
  ;; A heuristic "jump to definition" package
  ;; ggtags doesn't support R, dumb-jump is a decent substitute
  :after ess
  :hook (ess-mode . dumb-jump-mode)
  :config (setq dumb-jump-selector 'ivy)
  )

(use-package ggtags
  :bind (("H-g" . ggtags-mode))
  :config
  ;; Support languages: See https://www.gnu.org/software/global/
  ;; R is not supported (use dumb-jump)
  ;;
  ;; Prerequisites:
  ;; ggtags(global), ctags, pygments(for python support)
  ;; pygment is *not* enabled by default. We must provide ~/.globalrc to enable it.
  ;; Use /usr/share/gtags/gtags.conf should suffice.

  ;; Usage:
  ;; M-x ggtags-mode on any file in the project.
  ;; Use M-. to find definition, M-, to pop back.
  ;; Once found, will enter ggtags-navigation-mode. Use M-n, M-p to
  ;; navigate through errors. RET to exit ggtags-navigation-mode. With
  ;; projectile, use projectile-regenerate-tags to refresh tags.
  (add-hook 'python-mode-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'web-mode 'python-mode 'jsx-mode)
                (ggtags-mode 1))))
  )

;;;; LaTeX

(use-package tex
  ;; We're using AUCTeX, but the library that needs to be loaded is called tex
  ;; rather than auctex..
  ;; Corresponding mode-hook is called TeX-mode-hook and LaTeX-mode-hook. Note the
  ;; capitalization
  :straight auctex
  :straight reftex
  :straight magic-latex-buffer
  :straight company-auctex
  :hook (LaTeX-mode . magic-latex-buffer) ; Auto-enable fancy display of symbols.
  :hook (LaTeX-mode . flyspell-mode)
  :hook (LaTeX-mode . (lambda () (define-key LaTeX-mode-map "\C-xn" nil))) ;; see narrow-or-widen-dwim
  :config
  ;; Enable company-auctex
  (company-auctex-init)
  ;; Commonly-used:
  ;; C-c C-a to compile and preview slides
  )

;;;; Templating: Yasnippet

(use-package yasnippet
  :straight yasnippet-snippets
  :defer 5
  :bind (("C-c y" . 'yas/insert-snippet))
  :config
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "yasnippets" my-private-conf-directory))
  (yas-global-mode)
  (yasnippet-snippets-initialize)
  )

;;;; Syntax Checking: Flycheck

(use-package flycheck
  :defer 5
  :straight hydra
  :straight posframe
  :straight flycheck-posframe
  :bind ("C-z !" . hydra-flycheck/body)
  :bind (:map flycheck-mode-map
              ("s-p" . 'flycheck-previous-error)
              ("s-n" . 'flycheck-next-error)
              ("s-c" . 'flycheck-buffer)
              ("s-l" . 'flycheck-list-errors)
              )
  :hook ((text-mode prog-mode) . flycheck-mode) ;; Auto enable flycheck on programming modes
  :hook (flycheck-mode . flycheck-posframe-mode) ;; Show error on posframe
  :config
  ;; Adjust flycheck eagerness
  (defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
  This lets us fix any errors as quickly as possible, but in a
  clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))
  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)

  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))

  ;; Textlint setup.
  ;; npm install --global \
  ;; textlint-rule-write-good textlint-rule-max-comma textlint-rule-terminology\
  ;; textlint-rule-period-in-list-item textlint-rule-abbr-within-parentheses\
  ;; textlint-rule-en-max-word-count textlint-rule-diacritics\
  ;; textlint-rule-stop-words textlint-rule-ja-space-between-half-and-full-width
  ;; Check with plugins as well. See doc/languages.rst
  (setq flycheck-textlint-config (expand-file-name "textlintrc.json"
                                                   my-private-conf-directory))

  ;; Hydra
  (defhydra hydra-flycheck (:color blue)
    "
           ^
           ^Flycheck^          ^Errors^            ^Checker^
           ^────────^──────────^──────^────────────^───────^─────
           _q_ quit            _p_ previous        _?_ describe
           _M_ manual          _n_ next            _d_ disable
           _v_ verify setup    _f_ check           _m_ mode
           ^^                  _l_ list            _s_ select
           ^^                  ^^                  ^^
           "
    ("q" nil)
    ("p" flycheck-previous-error :color pink)
    ("n" flycheck-next-error :color pink)
    ("?" flycheck-describe-checker)
    ("M" flycheck-manual)
    ("d" flycheck-disable-checker)
    ("f" flycheck-buffer)
    ("l" flycheck-list-errors)
    ("m" flycheck-mode)
    ("s" flycheck-select-checker)
    ("v" flycheck-verify-setup))
  )
;;;; Language Server Protocol
(use-package lsp-mode
  :straight t
  :straight lsp-ui
  :straight company-lsp
  :after company
  :commands lsp-ui-mode
  :hook (prog-mode . lsp) ; Start LSP server in prog-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (push 'company-lsp company-backends)
  )

;;;; C/C++
;; [[https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/][Reddit post on creating C++ IDE in Emacs]].
(use-package ccls
  ;; C lsp backend
  ;; Requires binary ccls
  :after projectile
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers)
  )

;; TODO: Try with rtags, people say good
;; things about it. https://github.com/Andersbakken/rtags

;;;; Jupyter

(use-package jupyter
  ;; emacs-jupyter: provides connection to jupyter kernels directly.  This is
  ;; different from EIN, which aims to be a complete frontend instead.  To
  ;; connect to remote kernel (which is normal case during development), and
  ;; when SSH is available, use emacs-jupyter. If only web frontend is
  ;; available, use EIN.
  :init
  (straight-use-package 'zmq)
  :if (equal system-type 'gnu/linux)
  :defer 5
  :after org
  :config
  ;; ob-jupyter integration is added in org settings.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jupyter . t)))
  )

;;;; Python
(setq-default
 ;; Don't warn me when guessing indent
 python-indent-guess-indent-offset-verbose nil
 ;; Don't enable native readline completion
 python-shell-completion-native-enable nil)

(use-package elpy
  :mode ("\\.py\\'" . python-mode)
  :hook (elpy-mode . electric-indent-local-mode)
  :config
  (elpy-enable)
  )

(use-package blacken
  ;; Reformat python buffers using the "black" formatter
  :config
  (blacken-mode 1)
  )

(use-package ein
  ;; Jupyter Notebook in Emacs
  :bind (("H-j" . 'ein:notebooklist-login)
         ("H-h" . 'ein:jupyterhub-connect)
         ("H-J" . 'ein:jupyter-server-start))
  :commands (ein:jupyter-server-start ein:jupyterhub-connect)
  :init
  (straight-use-package '(simple-httpd :host github
                                       :repo "skeeto/emacs-web-server"
                                       :local-repo "simple-httpd"))
  :hook (ein:notebook-mode . visual-line-mode)
  :config
  (setq ein:worksheet-enable-undo 't)
  )

;;;; R

(use-package polymode
  :straight t
  :straight poly-markdown ; RMarkdown support
  :mode ("\\.Rmd" . poly-markdown-mode)
  )

(use-package ess
  :defer 5
  ;; NOTE: When using with flycheck-lintr-caching = t (default), make sure
  ;; ~/.R/lintr_cache directory is created.
  :straight t
  :config
  (setq  ess-default-style 'RStudio ;; Default code style: RStudio
         ess-tab-complete-in-script t ;; Tries to complete in script buffers
         ess-eldoc-show-on-symbol 'nil
         ess-indent-with-fancy-comments 'nil  ;; Don't distinguish between #, ## and ###
         ess-smart-S-assign-key 'nil ;; Disabled auto replace of "_" to "<-"
         ess-directory 'nil ;; By default starts ESS at current buffer default directory
         ess-ask-for-ess-directory nil
         ess-user-full-name "Cheong Yiu Fung")

  (defun cyf/ess-style-current-file ()
    (interactive)
    (if (string= ess-dialect "R")
        (ess-eval-linewise (concat "styler::style_file(\""
                                   buffer-file-name "\")"))))
  (defun cyf/ess-style-current-dir ()
    (interactive)
    (if (string= ess-dialect "R")
        (ess-eval-linewise (concat "styler::style_dir(\""
                                   default-directory "\")"))))
  )

;;;; Lisp

(use-package elisp-mode
  :straight nil
  :init
  (defun bozhidar-visit-ielm ()
    "Switch to default `ielm' buffer.
      Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-z" . bozhidar-visit-ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer))
  )

(use-package elisp-slime-nav
  ;; Convenient navigation to symbol at point
  ;; Try M-. and M-,
  :hook ((emacs-lisp-mode-hook ielm-mode-hook) . elisp-slime-nav-mode)
  )

;; Check the great gist at
;; https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
;; And this tutorial: https://ebzzry.io/en/emacs-pairs/
(use-package smartparens
  :defer 5
  :bind (:map smartparens-mode-map
              ("M-("           . sp-wrap-round)
              ("M-["           . sp-wrap-square)
              ("M-{"           . sp-wrap-curly)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("M-<del>"       . sp-unwrap-sexp)
              ("C-<right>"     . sp-forward-slurp-sexp)
              ("C-<left>"      . sp-backward-slurp-sexp)
              ("C-M-<right>"   . sp-forward-barf-sexp)
              ("C-M-<left>"    . sp-backward-barf-sexp)
              ("C-M-a"         . sp-beginning-of-sexp)
              ("C-M-e"         . sp-end-of-sexp))
  :config
  (require 'smartparens-config)
  (--each '(css-mode-hook
            restclient-mode-hook
            js-mode-hook
            java-mode
            emacs-lisp-mode-hook
            ruby-mode
            ;; org-mode-hook
            ess-mode-hook
            markdown-mode
            groovy-mode
            scala-mode)
    (add-hook it 'turn-on-smartparens-strict-mode))
  )

;;;; Shell

(advice-add 'sh-set-shell :around #'suppress-messages)

;;;; Haskell

(use-package haskell-mode
  ;; Haskell support
  :straight t
  :straight intero
  :config
  (intero-global-mode 1)
  )

;;;; Scala

(use-package ensime
  ;; Scala IDE
  :straight (:host github :repo "ensime/ensime-emacs" :branch "2.0")
  )

;;;; Julia
(use-package julia-mode)
;;;; PHP

(use-package php-mode)

;;;; SQL
(use-package sql-indent
  ;; SQL
  ;; To work with flycheck, install gem install --user-install sqlint
  :hook (sql-mode . sqlind-minor-mode)
  )

;;;; Markdown

(use-package markdown-mode
  :straight markdown-toc
  :straight vmd-mode ; For GFM preview
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md" . gfm-mode)
         ("*\\.md" . markdown-mode)
         ("*\\.markdown" . markdown-mode))
  :bind (:map markdown-mode-map
              (("M-<up>" . markdown-move-up)
               ("M-<down>" . markdown-move-down)
               ("M-<left>" . markdown-promote)
               ("M-<right>" . markdown-demote)))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-math t
        markdown-asymmetric-header t)
  )

;;;; HTTP

(use-package restclient
  :straight t
  :straight ob-restclient
  :straight company-restclient
  :after company
  :defer 10
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t)))

  (add-to-list 'company-backends 'company-restclient)
  )

;;;; Drawing Graphs: Graphviz + PlantUML

(use-package graphviz-dot-mode
  :config
  ;; https://stackoverflow.com/questions/16770868/org-babel-doesnt-load-graphviz-editing-mode-for-dot-sources
  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
  )

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :config
  (setq plantuml-jar-path (expand-file-name "bin/plantuml.jar" my-emacs-conf-directory))
  )

;;;; Misc programming supporting modes

(use-package ahk-mode
  :mode "\\.ahk\\'")
(use-package json-mode
  ;; JavaScript
  :mode ("\\.json\\'" . json-mode)
  :bind (("s-j" . json-mode-beautify))
  :config
  ;; pretty print json, putting array in the same line
  (defun encode-json-array-of-numbers-on-one-line (encode array)
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not (loop for x across array always (numberp x)))))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array)))
  (advice-add 'json-encode-array :around #'encode-json-array-of-numbers-on-one-line))
(use-package rainbow-mode
  ;; Prettify CSS colors in-place
  )
(use-package csv-mode
  ;; CSV
  )
(use-package yaml-mode)

;;; Code Searching/Replacing

(use-package color-moccur
  ;; :commands (moccur)
  ;; TODO
  :bind (("C-s o" . 'moccur)
         ;; :map isearch-mode-map
         ;; ("C-s o" . isearch-moccur)
         ;; ("C-s O" . isearch-moccur-all)
         )
  )

(use-package grep
  ;; Emacs native grepping commands
  :bind (("C-s G" . grep)  ; Run grep
         ("C-s d" . find-grep-dired) ; REGEXP search files in directory
         ("C-s n" . find-name-dired) ; Globbing search files in directory
         ("C-s R" . rgrep) ; Recursively grep files in directory tree
         ))

(use-package wgrep
  ;; Writable grep results
  :straight wgrep-helm
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key "e")
  )

(use-package rg
  :bind ("C-s s" . rg-dwim))

(use-package deadgrep
  :bind(("C-s g" . deadgrep)))

;;; General Keybindings

;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Personal useful key-bindings
(global-set-key (kbd "C-c R") 'revert-buffer)
(global-set-key (kbd "C-c O") 'occur)
(global-set-key (kbd "M-H") 'mark-paragraph)

;; Hyper key for application shortcuts
(global-set-key (kbd "H-c") 'calc)
(global-set-key (kbd "H-e") 'mu4e)
(global-set-key (kbd "H-t") 'load-theme)
(global-set-key (kbd "<XF86Open>") 'ivy-switch-buffer)

;; <f1> for help-* commands
(global-set-key (kbd "<f2>") 'counsel-find-file-extern)
;; <f3> <f4> for macro
(global-set-key (kbd "<f5>") 'org-agenda-list)
;; <f6> for ivy-resume
(global-set-key (kbd "<f7>") 'follow-delete-other-windows-and-split)
(global-set-key (kbd "<f8>") 'follow-mode)
;; <f9> - <f12> for eyebrowse workspace

;; Remap next layout and previous layout
(global-set-key (kbd "M-J") (lambda ()
                              (interactive)
                              (join-line -1)))

;;; Appearance
;;;; Modeline
;; Show time on mode line, but don't show system load average.
(setq display-time-default-load-average 'nil)
(display-time-mode +1)

(use-package minions
  :bind ("<mouse-3>" . minions-minor-modes-menu)
  :defer 2
  :init
  (setq-default minions-mode-line-lighter ";") ;; A better smile :)
  :config
  (minions-mode 1))

(use-package hide-mode-line
  :bind ("C-c H" . hide-mode-line-mode))

;;;; Defuns for themes customization

;; Add directory for customized themes
(setq custom-theme-directory (expand-file-name "custom-themes" my-private-conf-directory))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(defun cyf/theme-type ()
  "Check current theme type by luminance. If luminance is larger than
 0.7, return 'light, else return 'dark."
  (let* (
         (bg-rgb (color-name-to-rgb (frame-parameter nil 'background-color)))
         (hsl (apply 'color-rgb-to-hsl bg-rgb))
         (luminance (nth 2 hsl))
         )
    (if (> luminance 0.7)
        'light
      'dark)))

(defun cyf/set-org-todo-keyword-faces ()
  "Set org-todo keyword faces for themes. Two sets of faces are
   provided: \"dark\" or \"light\". It will be automatically set based
   on value of (cyf/theme-type). "
  (interactive)
  (cond ((equal 'dark (cyf/theme-type))
         (progn
           (setq org-todo-keyword-faces
                 '(("TODO" . "dark khaki")
                   ("NEXT" . "dark salmon")
                   ("IN-PROGRESS" . "dark cyan")
                   ("WAIT" . "dark goldenrod")
                   ("BLOCKED" . "dark red")
                   ("SOMEDAY" . "DarkSlateGray4")
                   ("CANCELLED" . "dark grey")
                   ("DONE" . "dark sea green")))
           (message "[cyf] Setting org-todo-keyword-faces to dark theme.. DONE")))
        ((equal 'light (cyf/theme-type))
         (progn
           (setq org-todo-keyword-faces
                 '(("TODO" . "black")
                   ("NEXT" . "rosy brown")
                   ("IN-PROGRESS" . "royal blue")
                   ("WAIT" . "sienna")
                   ("BLOCKED" . "red")
                   ("SOMEDAY" . "coral")
                   ("CANCELLED" . "dim grey")
                   ("DONE" . "medium sea green")))
           (message "[cyf] Setting org-todo-keyword-faces to light theme.. DONE")))))

(defun cyf/set-light-theme-background ()
  "White background for some themes hurts. Change it to yellow."
  (interactive)
  (if (equal my-cur-theme 'leuven)
      (progn
        (message "[cyf] Setting leuven backgrounds to floral white")
        (custom-theme-set-faces
         'leuven '(default ((t (:background "floral white")))))
        ;; (set-background-color "floral white")
        )
    ))

(defun cyf/set-dark-theme-highlight-region ()
  "In dark themes it's difficult to see where it highlights the
 texts. This fix it. "
  (interactive)
  (if (equal 'dark (cyf/theme-type))
      (set-face-attribute 'region nil :background "#666" :foreground "#ffffff"))
  )

(defun cyf/set-theme ()
  "Apply all settings in one batch."
  (interactive)
  (progn
    (cyf/set-org-todo-keyword-faces)
    (cyf/set-light-theme-background) ;; Some light themes have good default background
    (cyf/set-dark-theme-highlight-region)
    )
  )

;; Customize load-theme so we have a clean state each time we apply the theme.
(setq my-cur-theme 'nil)
(defun my-load-theme-before (THEME &optional NO-CONFIRM NO-ENABLE)
  (disable-theme my-cur-theme)
  )
(defun my-load-theme-after (THEME &optional NO-CONFIRM NO-ENABLE)
  (progn
    (cyf/set-theme)
    (setq my-cur-theme THEME)
    )
  )
(advice-add #'load-theme :before #'my-load-theme-before)
(advice-add #'load-theme :after #'my-load-theme-after)

;;;; Themes

(use-package color-theme-sanityinc-tomorrow)
(use-package kaolin-themes)
(use-package parchment-theme)
(use-package emacs-one-themes
  :straight (:host github :repo "balajisivaraman/emacs-one-themes"))

(load-theme 'kaolin-light t)

(use-package solaire-mode
  ;; visually distinguish file-visiting windows from other types of windows (like popups or sidebars) by giving them a
  ;; slightly different -- often brighter -- background
  :defer 3
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg)
  (solaire-global-mode 1))

;;;; Fonts

(defun cyf/set-fonts ()
  (interactive)
  ;; Default
  (set-face-attribute
   'default nil
   :font (font-spec :name "Sarasa Mono TC"
                    :weight 'normal
                    :slant 'normal
                    :size 11.5))
  ;; Fixed-width for programming
  (set-face-attribute
   'fixed-pitch nil
   :font (font-spec :name "Sarasa Mono TC"
                    :weight 'normal
                    :slant 'normal
                    :size 11.5))
  ;; Variable-width for reading
  (set-face-attribute
   'variable-pitch nil
   :font (font-spec :name "Bookerly"
                    :weight 'normal
                    :slant 'normal
                    :size 12.0))
  ;; For all CJK fonts
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "Sarasa Mono TC"
                :weight 'normal
                :slant 'normal
                :size 11.5)))
  )
;; Set fonts every time a new Window frame is created.
(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                   (cyf/set-fonts))))
;; Immediately run if we start emacs directly without daemon
(if window-system
    (cyf/set-fonts))

(use-package mixed-pitch
  :disabled
  ;; :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-variable-pitch-cursor 'nil)
  (mapc (lambda (face)
          (add-to-list 'mixed-pitch-fixed-pitch-faces face))
        (list 'org-code
              'org-link
              'org-block
              'org-table
              'org-verbatim
              'org-block-begin-line
              'org-block-end-line
              'org-meta-line
              'org-document-info-keyword))
  )

;;; Misc Tools

(use-package olivetti
  :defer 3
  ;; :hook (org-mode . olivetti-mode)
  ;; :hook (org-agenda-mode . olivetti-mode)
  :config
  (setq-default olivetti-body-width 0.9)
  )

(use-package dtk
  :defer 3
  :straight (:host github :repo "dtk01/dtk" :branch "master")
  :init
  (straight-use-package '(sword-to-org :host github :repo "alphapapa/sword-to-org"))
  :bind (("C-c B" . dtk-bible))
  :custom (dtk-word-wrap t)
  (dtk-default-module "ChiUn")
  (dtk-default-module-category "Biblical Texts")
  ;; Bible reading.
  ;; Install sword and xiphos. Use xiphos to download modules.
  ;; :hook (dtk-mode . auto-fill-mode) ;; Not implemented yet
  )

(use-package alert
  ;; Notification
  :config
  (setq alert-default-style 'libnotify))

(use-package lorem-ipsum
  :defer 3
  )

(use-package calfw
  :straight t
  :straight calfw-org
  :straight calfw-cal
  :bind (:map cfw:calendar-mode-map
              ("M-n" . cfw:navi-next-month-command)
              ("M-p" . cfw:navi-previous-month-command))
  :defer 5
  :config
  (require 'calfw-cal)
  (require 'calfw-org)
  (defun my-calendar ()
    (interactive)
    (let ((buf (get-buffer "*cfw-calendar*")))
      (if buf
          (pop-to-buffer buf nil)
        (cfw:open-calendar-buffer
         :contents-sources
         (list (cfw:org-create-source "Dark Blue")
               (cfw:cal-create-source "Dark Orange"))
         :view 'two-weeks)))
    )

  (setq cfw:org-overwrite-default-keybinding t)
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)
  )

(use-package excorporate
  :defer t
  :after (org calfw)
  :config
  (setq-default excorporate-configuration '((auth-source-pass-get "email" "outlook365")
                                            . "https://outlook.office365.com/EWS/Exchange.asmx")
                excorporate-diary-today-file (expand-file-name
                                              "excorporate/diary-excorporate-today" org-directory)
                excorporate-diary-transient-file (expand-file-name
                                                  "excorporate/diary-excorporate-transient"
                                                  org-directory)
                ;; Press e and show exco in calfw
                excorporate-calendar-show-day-function 'exco-org-show-day
                ;; Show meetings in org agenda
                org-agenda-include-diary t)

  ;; Make sure that Emacs diary knows how to follow `#include "..."'
  ;; directives (needed by excorporate)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  (excorporate-diary-enable)
  )

(use-package operate-on-number
  :bind ("C-c N" . operate-on-number-at-point))

(use-package elfeed
  ;; RSS Reader
  :defer 3
  :straight t
  :straight elfeed-org
  :bind (("C-c e" . bjm/elfeed-load-db-and-open)
         :map elfeed-search-mode-map
         ("q" . bjm/elfeed-save-db-and-bury))
  :init
  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))
  ;;write to disk when quitting
  (defun bjm/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" my-private-conf-directory)))
  (setq elfeed-db-directory (expand-file-name "elfeed" my-private-conf-directory))

  (elfeed-org)
  )

(use-package docker
  :straight dockerfile-mode
  :bind ("H-d" . docker))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package keyfreq
  ;; Track key frequencies
  :config
  (keyfreq-mode 1))

(use-package beancount
  :straight nil
  :mode ("\\.beancount\\'" . beancount-mode)
  :bind (:map beancount-mode-map
              ;; Originally uses crux to cleanup buffer, but not quite useful for beancount so we rebind
              ("C-c n" . (lambda () (interactive) (beancount-align-numbers (point-min) (point-max))))
              )
  :defer 3
  :config
  (setq-default beancount-use-ido 'nil)
  ;; TIP: Use yasnippet to quickly insert a transaction
  )

(use-package calc
  ;; Calculator
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit")))
  :config
  (setq math-units-table nil))

(use-package demo-it
  ;; Presentation within Emacs
  )

(use-package atomic-chrome
  ;; Edit with Chrome/Firefox
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("reddit.com" . markdown-mode)))
  (setq atomic-chrome-buffer-open-style 'frame)
  )

(use-package google-translate
  :defer 10
  :bind (
         ("C-c T" . 'google-translate-query-translate)
         )
  :config
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh-CN"
        google-translate-pop-up-buffer-set-focus t
        google-translate-output-destination 'echo-area
        google-translate-show-phonetic t
        google-translate-translation-directions-alist '(("cn" . "en")))
  )

(use-package erc
  ;; Internet Relay Chat (IRC)
  :straight erc-hl-nicks
  :straight erc-image
  :defer 3
  :commands (erc erc-tls)
  :custom-face (erc-timestamp-face ((t (:foreground "DarkSlateGrey")))) ; Don't know why it's green everywhere
  :config
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#archlinux" "#bash" "#emacs" "#latex" "#org-mode" "#python" "#xmonad"))
        erc-autojoin-timing 'ident
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477")
        erc-timestamp-only-if-changed-flag t
        erc-timestamp-format "%H:%M "
        erc-fill-column 78
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 18
        erc-prompt-for-nickserv-password 'nil)

  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules)
  (erc-track-minor-mode 1)
  (erc-track-mode 1)
  (erc-hl-nicks-mode 1)

  (defun irc ()
    "Connects to ERC, or switch to last active buffer."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (erc-track-switch-buffer 1)
      (erc :server (auth-source-pass-get "host" "erc")
           :nick (auth-source-pass-get "nick" "erc")
           :password (auth-source-pass-get 'secret "erc")))))

(use-package gif-screencast
  ;; Record Emacs Screen. Requires scrot, convert(magick) and gifsicle.  Use
  ;; screenkey to show key pressed.
  :bind (("<C-print>" . gif-screencast)
         ("<C-pause>" . gif-screencast-stop))
  :config
  (setq gif-screencast-output-directory (expand-file-name "images/gif-screencast" org-directory))
  )

(use-package slack
  :commands (slack-start)
  :bind (
         ;; Track changed buffers. Use this to check updates from channels, groups and ims!
         ("<s-tab>" . tracking-next-buffer)
         :prefix "C-c M" ;; message
         :prefix-map my-slack-map
         ("M" . slack-start)
         ("i" . slack-im-select)
         ("g" . slack-group-select)
         ("c" . slack-channel-select)
         ("@" . slack-message-embed-mention)
         :map slack-message-buffer-mode-map
         ("C-c C-a" . 'slack-message-add-reaction)
         ("C-c C-r" . 'slack-message-remove-reaction)
         )
  :init
  (setq slack-buffer-emojify t ;; if you want to enable emoji, default nil
        slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "data science aswatson"
   :default t
   :client-id (auth-source-pass-get "client-id" "slack")
   :client-secret (auth-source-pass-get "client-secret" "slack")
   :token (auth-source-pass-get "token" "slack")
   ;;:full-and-display-names t
   )
  )

(defun crontab-e ()
  "crontab within Emacs."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(use-package prodigy
  ;; Manage services
  :defer 10
  :bind (("C-c s" . prodigy))
  :config
  (prodigy-define-service
    :name "personal ledger"
    :command "fava"
    :path '("~/.local/bin/")
    :args `(,(eval org-my-beancount-file))
    :tags '(home)
    )
  (prodigy-define-service
    :name "Local Jupyter"
    :command "jupyter"
    :args '("notebook")
    ;; Jupyter will need to confirm on exit. Never mind, just kill it
    :stop-signal 'sigkill
    :tags '(work)
    )
  (prodigy-define-service
    :name "aria2"
    :command "aria2c"
    :tags '(it-actually-works)
    )
  (prodigy-define-service
    :name "blog preview"
    :command "hugo"
    :args '("server" "--disableFastRender" "-D")
    :cwd "/home/yiufung/Dropbox/blog/"
    :tags '(home work)
    )
  )

;;; Start Emacs server

(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
