;;; init.el --- yiufung's config  -*- lexical-binding: t; coding:utf-8; fill-column: 119 -*-

;;; Commentary:
;; My personal config. Use `outshine-cycle-buffer' (<S-Tab>) to navigate through sections, and `counsel-imenu' (C-c i)
;; to locate individual use-package definition.

;;; Code:

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
      find-file-visit-truename t
      ;; Avoid stale compiled code shadow newer source code
      load-prefer-newer t)

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

  (defun lookup-password (host user port)
    (require 'auth-source)
    (require 'auth-source-pass)
    (let ((auth (auth-source-search :host host :user user :port port)))
      (if auth
          (let ((secretf (plist-get (car auth) :secret)))
            (if secretf
                (funcall secretf)
              (error "Auth entry for %s@%s:%s has no secret!"
                     user host port)))
        (error "No auth entry found for %s@%s:%s" user host port))))

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

  ;; Need to set allow allow-emacs-pinentry & allow-loopback-pinentry in ~/.gnupg/gpg-agent.conf
  (setq epa-pinentry-mode 'loopback)
  ;; Top debug, set auth-source-debug to t.
  (setq auth-source-debug t)
  ;; also use auth-source-forget-all-cached
  )


;;;; General settings
(setq-default ;; Use setq-default to define global default
 ;; Who I am
 user-mail-address "mail@yiufung.net"
 user-full-name "Cheong Yiu Fung"
 ;; Enable all disabled commands
 disabled-command-function nil
 ;; Enable recursive minibuffer edit
 enable-recursive-minibuffers t
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
 ;; Leave some rooms when recentering to top, useful in emacs ipython notebook.
 recenter-positions '(middle 1 bottom)
 ;; Move files to trash when deleting
 delete-by-moving-to-trash t
 ;; Show column number
 column-number-mode t
 ;; Don't break lines for me, please
 truncate-lines t
 ;; More message logs
 message-log-max 16384
 ;; Don't prompt up file dialog when click with mouse
 use-file-dialog nil
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
 ;; Reserve one line when scrolling
 scroll-margin 1
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
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
;; Don't blink
(blink-cursor-mode 0)

;; ESC is mapped as metakey by default, very counter-intuitive.
;; Map escape to cancel (like C-g) everywhere
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(define-key isearch-mode-map "\e" 'isearch-abort)   ;; \e seems to work better for terminals
(global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else
;; Let C-g works when cursor is in buffers other than minibuffer
;; https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))
(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

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

;; Confirm when trying to kill frame in emacsclient
;; TODO: Disable for now. It seems to clash with sdcv
;; (define-advice delete-frame (:around (oldfun &rest args) confirm-frame-deletion)
;;   "Confirm deleting the frame."
;;   (interactive)
;;   (when (y-or-n-p "Delete frame? ")
;;     (apply oldfun args)))

;; Early unbind keys for customization
(unbind-key "C-s") ; Reserve for search related commands
(unbind-key "C-z") ;; Reserve for hydra related commands

;; Quick access to commonly used files
(global-set-key (kbd "s-SPC") (lambda () (interactive) (find-file (expand-file-name ".emacs.d/init.el"
                                                                                    my-emacs-conf-directory))))
(global-set-key (kbd "s-<print>") (lambda () (interactive) (find-file "~/screenshots")))
(global-set-key (kbd "s-f") (lambda () (interactive) (find-file-other-window org-my-beancount-file)))

(use-package beacon
  ;; Highlight the cursor whenever it scrolls
  :defer 5
  :bind (("C-<f12>" . beacon-blink)) ;; useful when multiple windows
  :config
  (setq beacon-size 10)
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
   revert-without-query '("\\.pdf" "\\.png")
   ;; Auto reverting files in remote machine. Useful for remote plotting.
   auto-revert-remote-files t)
  (global-auto-revert-mode 1) ;; work with auto-save with Org files in Dropbox
  )

(use-package recentf
  :defer 5
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :hook (dired-mode . recentf-add-dired-directory)
  :config
  (setq recentf-save-file "~/.emacs.d/data/recentf"
        recentf-max-saved-items 'nil ;; Save the whole list
        recentf-max-menu-items 50
        ;; Cleanup list if idle for 10 secs
        recentf-auto-cleanup 60)

  ;; Suppress "Cleaning up the recentf...done (0 removed)"
  (advice-add 'recentf-cleanup :around #'suppress-messages)
  (recentf-mode 1)
  )

(use-package super-save
  :defer 3
  :config
  (setq auto-save-default nil)
  (super-save-mode +1))

(use-package aggressive-indent
  ;; Aggressive indent mode
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
  (setq which-key-idle-delay 1.0)
  (which-key-mode)
  )

(use-package whole-line-or-region
  ;; If no region is active, C-w M-w will act on current line
  :defer 5
  ;; Right click to paste: I don't use the popup menu a lot.
  :bind ("<mouse-3>" . whole-line-or-region-yank)
  :bind (:map whole-line-or-region-local-mode-map ("C-w" . kill-region-or-backward-word)) ;; Reserve for backward-kill-word
  :config
  (whole-line-or-region-global-mode)
  )

(use-package crux
  ;; A handful of useful functions
  :defer 1
  :bind (
         ("C-x t"      . 'crux-swap-windows)
         ("C-c b"      . 'crux-create-scratch-buffer)
         ("C-x o"      . 'crux-open-with)
         ("C-x f"      . 'crux-recentf-find-file)
         ("C-x 4 t"    . 'crux-transpose-windows)
         ("C-x C-k"    . 'crux-delete-buffer-and-file)
         ;; ("C-c n"      . 'crux-cleanup-buffer-or-region)
         ;; ("s-<return>" . 'crux-cleanup-buffer-or-region)
         ("C-M-y"      . 'crux-duplicate-and-comment-current-line-or-region)
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
  :disabled
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
  :straight visual-fill-column
  :defer 5
  :hook ((prog-mode) . auto-fill-mode)
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

  (defun remove-extra-blank-lines (&optional beg end)
    "If called with region active, replace multiple blank lines
with a single one.

Otherwise, call `delete-blank-lines'."
    (interactive)
    (if (region-active-p)
        (save-excursion
          (goto-char (region-beginning))
          (while (re-search-forward "^\\([[:blank:]]*\n\\)\\{2,\\}" (region-end) t)
            (replace-match "\n")
            (forward-char 1)))
      (delete-blank-lines)))

  (defun countdown ()
    "Show a message after timer expires. Based on run-at-time and can understand time like it can."
    (interactive)
    (let* ((msg-to-show (read-string "Message to show: "))
           (time-duration (read-string "Time: ")))
      (message time-duration)
      (run-at-time time-duration nil #'alert msg-to-show)))

  ;; Activate `visual-fill-column-mode' in every buffer that uses `visual-line-mode'
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (setq-default visual-fill-column-width 119
                visual-fill-column-center-text nil)
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
     ("“" "”")
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
  :straight t
  :straight visual-regexp-steroids
  :defer 5
  :bind (("C-c r" . 'vr/replace)
         ("C-c %"   . 'vr/query-replace))
  :config
  (global-set-key [remap query-replace] 'vr/query-replace) ; M-%
  ;; Build up regexp with different backend engine. This supports Emacs, Python, pcre2el. I mostly use it to build up
  ;; Python regexp but frequently forgot about the name, so an alias is set.
  (defalias 'regexp-visualize 'vr/select-replace)
  )

(use-package ediff
  :hook (ediff-prepare-buffer . outline-show-all) ;; Expand file contents, especially for Org files.
  :config
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

  ;; Do everything in one frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package undo-tree
  :defer 3
  :config
  (global-undo-tree-mode))

(use-package undo-propose
  :disabled
  :bind (("C-c u" . undo-propose))
  )

;; Save undo history across sessions
;; TODO: Annoying prompts in magit
;; (require 'undohist)
;; (undohist-initialize)

(use-package unfill
  :bind (("M-Q" . unfill-paragraph)
         ("M-W" . copy-region-as-kill-unfilled))
  :commands (unfill-paragraph unfill-region unfill-toggle)
  :config
  (defun copy-region-as-kill-unfilled (beg end)
    "Copy region, but with all paragraphs unfilled.

Useful when hard line wraps are unwanted (email/sharing article)."
    (interactive "r")
    (copy-region-as-kill beg end)
    (with-temp-buffer
      (yank)
      (mark-whole-buffer)
      (unfill-paragraph)
      (kill-region (point-min) (point-max))))
  )

;;; Completion Framework: Ivy / Swiper / Counsel
(use-package counsel
  :demand t
  :straight ivy-hydra
  :straight ivy-rich
  :straight counsel-projectile
  :straight ivy-posframe
  :straight smex
  :straight (flx :repo "lewang/flx" :host github :files ("flx.el"))
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
         ;; C-c C-o opens an occur buffer
         ;; e to toggle writable state
         ("C-s C-s" . counsel-ag)
         ("C-s r"   . counsel-rg)
         ("C-s f"   . counsel-file-jump) ;; Jump to a file below the current directory.
         ("C-s j"   . counsel-dired-jump);; Jump to directory under current directory
         )
  :init
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 50))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))  ; thr original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file
  :config
  (ivy-mode 1)
  (ivy-rich-mode 1)
  (counsel-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (counsel-projectile-mode 1)
  (setq smex-save-file (expand-file-name "smex-items" my-private-conf-directory))
  (setq ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers nil ;; don't show recent files/bookmarks as buffers in C-x b
        ivy-use-selectable-prompt t ;; C-M-j to rename similar filenames
        enable-recursive-minibuffers nil
        ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-count-format "(%d/%d) "
        ;; Useful settings for long action lists
        ;; See https://github.com/tmalsburg/helm-bibtex/issues/275#issuecomment-452572909
        max-mini-window-height 0.30
        ;; Don't parse remote files, it's slow
        ivy-rich-parse-remote-buffer 'nil)

  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
  ;; (ivy-posframe-mode 1)
  )

;;; File Nav & Mgmt: Follow / Dired / Bookmark+

(use-package follow
  :bind (("<f7>" . follow-delete-other-windows-and-split)
         ("<f8>" . follow-mode))
  )

(use-package dired
  :defer 3
  :straight async
  :straight dired-du ;; Only enable when needed
  :straight dired-git-info ;; Show last git commit message alongside with file
  :straight diredfl ;; Colorful columns
  :straight dired-hacks ;; some utilities function
  :straight dired-rsync ;; Large file synchronization
  :hook ((dired-mode . dired-hide-details-mode) ;; Hide change times etc
         (dired-mode . dired-omit-mode)) ;; Hide dot files
  :bind (("C-x C-d" . dired)  ;; Original list-directory is not useful.
         :map dired-mode-map
         ("C-x M-h" . dired-du--toggle-human-readable)
         ("C-c C-r" . dired-rsync)
         ("M-k" . dired-kill-subdir)
         (")" . dired-git-info-mode))
  :config
  (require 'dired-x) ;; extra functionality for dired
  (setq dired-listing-switches "-aBhl --group-directories-first"
        dired-dwim-target t
        dired-no-confirm '(copy)
        dired-du-bind-human-toggle 'nil) ;; C-h is remapped in my config. Map it to M-h.
  (diredfl-global-mode 1)

  ;; Fix copy error over TRAMP in dired-async-mode. See async-manual.
  (setq async-quiet-switch "-q")
  (dired-async-mode 1)

  ;; Omit dotfiles.
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
        dired-omit-verbose nil)

  (defvar dired-compress-files-alist
    '(("\\.tar\\.gz\\'" . "tar -c %i | gzip -c9 > %o")
      ("\\.zip\\'" . "zip %o -r --filesync %i"))
    "Control the compression shell command for `dired-do-compress-to'.

Each element is (REGEXP . CMD), where REGEXP is the name of the
archive to which you want to compress, and CMD the the
corresponding command.

Within CMD, %i denotes the input file(s), and %o denotes the
output file. %i path(s) are relative, while %o is absolute.")

  ;; Enable directory collapsing behavior just like GitHub
  (dired-collapse-mode)
  )

(use-package bookmark-plus
  ;; Bookmark utilities
  :straight (bookmark-plus :type git :host github :repo "emacsmirror/bookmark-plus")
  :defer 3
  :init
  (require 'bookmark+)
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

(use-package windmove
  :straight nil
  :bind (
         ("s-j" . windmove-down)
         ("s-k" . windmove-up)
         ("s-h" . windmove-left)
         ("s-l" . windmove-right)
         )
  )

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
  :bind (("<C-return>" . ace-window))
  :custom-face (aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
  :config
  (setq
   ;; Home row is more convenient. Use home row keys that prioritize fingers that don't move.
   aw-keys '(?j ?k ?l ?f ?d ?s ?g ?h ?\; ?a)
   aw-scope 'visible)
  )

(use-package winner
  ;; Enable window restoration
  :defer 1
  :config
  (winner-mode 1))

(setq
 ;; Kill a frame when quitting its only window
 frame-auto-hide-function 'delete-frame
 ;; Maximum number of side-windows to create on (left top right bottom)
 window-sides-slots '(0 1 2 2)
 ;; Default rules
 display-buffer-alist
 `(;; Right side for most Help, Agenda, Trace, etc buffers
   ("*\\(Help\\|help\\|Man.*\\|trace-\\|Backtrace\\|RefTeX.*\\|ess-describe\\|SDCV.*\\| Merriam.*\\)"
    (display-buffer-reuse-mode-window display-buffer-in-previous-window display-buffer-in-side-window)
    (side . right)
    (slot . 1)
    (window-width . 80)
    (window-height . 0.7)
    (reusable-frames . visible))
   ;; Same window
   ("*\\(R.*\\|Python\\)"
    (display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . visible))
   ;; Show on bottom
   ("*\\(ielm\\)"
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (window-height . 10)
    (reusable-frames . visible))
   ("^\\vterm"
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (slot . 2)
    (window-width . 80)
    (reusable-frames . visible))
   ;; Always show notmuch in new frame
   ("^\\*info"
    (display-buffer-reuse-window display-buffer-in-previous-window))
   ;; Display *BBDB* buffer on the bottom frame
   ("\\*BBDB"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (window-height . 10)
    (reusable-frames . visible))
   ;; Split shells at the bottom
   ("^\\*e?shell"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-below-selected)
    (window-min-height . 20)
    (reusable-frames . visible)
    )
   )
 )

(use-package nswbuff
  ;; Quickly switching buffers. Quite useful!
  :bind (("<C-tab>"           . nswbuff-switch-to-next-buffer)
         ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
  :config
  (setq nswbuff-display-intermediate-buffers t)
  )

;;; Cursor Navigation: avy

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

;; A pacman hook is to used to update nodejieba whenever necessary
(use-package jieba
  :straight (:host github :repo "cireu/jieba.el" :files (:defaults "*.js")) ;; Need to symlink js for server too
  :defer 3
  :config
  (jieba-mode))

;;; Version-control: Magit

(use-package magit
  :defer 10
  :straight gitignore-templates
  :straight diff-hl
  :straight git-timemachine
  ;;display flycheck errors only on added/modified lines
  :straight magit-todos
  :straight magit-diff-flycheck
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
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-clone-set-remote.pushDefault nil
        magit-clone-default-directory "~/projects/")

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  ;; Set magit password authentication source to auth-source
  (add-to-list 'magit-process-find-password-functions
               'magit-process-password-auth-source)

  ;; Solve a long-standing issue where magit complains about existence of index.lock.
  ;; See https://emacs.stackexchange.com/questions/40917/how-can-i-get-a-prompt-for-deleting-index-lock-file-in-magit
  (defun magit-remove-git-lock-file ()
    "Remove git's index lock file, if it exists."
    (interactive)
    (let ((base (magit-toplevel)))
      (delete-file (concat base "/.git/index.lock"))))
  )

(use-package monky
  ;; Mercurial support
  )

;;; Workspace Mgmt: eyebrowse + projectile

(use-package projectile
  :defer 5
  :straight ripgrep ;; required by projectile-ripgrep
  :bind-keymap
  ("C-c P" . projectile-command-map)
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
   ("C-c w s"   . 'eyebrowse-switch-to-window-config)
   ("C-c w k"   . 'eyebrowse-close-window-config)
   ("C-c w w"   . 'eyebrowse-last-window-config)
   ("C-c w n"   . 'eyebrowse-next-window-config)
   ("C-c w p"   . 'eyebrowse-prev-window-config)
   ("C-c n"     . 'eyebrowse-next-window-config)
   ("C-c p"     . 'eyebrowse-prev-window-config)
   ("C-c '"     . 'eyebrowse-switch-to-window-config))
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-close-window-config-prompt t
        eyebrowse-mode-line-style 'smart
        eyebrowse-tagged-slot-format "%t"
        eyebrowse-new-workspace nil)
  (eyebrowse-mode)
  )

;;; Org

(use-package org
  ;; Combining demand and org-plus-contrib to ensure the latest version of org is used
  :demand t
  :straight org-plus-contrib
  :straight ob-ipython
  :straight ob-async
  :straight ob-mermaid
  :straight ob-http
  :straight org-bullets
  :straight org-super-agenda
  :straight org-pomodoro
  :straight org-sidebar
  :straight org-present
  :straight org-msg
  :straight org-chef
  :straight ox-clip
  :straight ox-twbs
  :straight ox-tufte
  :straight org-cliplink
  :straight ox-gfm
  :straight org-download
  :straight ox-hugo
  :straight easy-hugo
  :straight gnuplot
  :straight helm-org-rifle
  :hook (org-mode . org-bullets-mode)
  :hook (org-mode . org-indent-mode)
  :hook (org-mode . turn-off-auto-fill)
  :hook (org-mode . visual-line-mode)
  :init
  ;; customized export formats
  (straight-use-package '(ox-ipynb :host github :repo "jkitchin/ox-ipynb"))
  :bind (
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c 0" . org-set-created-property)
         ("s-`"   . org-clock-goto) ;; Jump to currently clocking headline
         ;; Rifle through all my org files to identify an item.
         ;; Use C-s to display results in occur-like style.
         ("C-S-s" . helm-org-rifle)
         ("s-e" . ivy-insert-org-entity)
         ("H-p" . org-pomodoro)
         :map org-mode-map
         ("C-x n s" . nil)
         ("C-x n b" . nil)
         ("C-x n e" . nil)
         ("C-x n"   . nil)
         ("C-c C-j" . counsel-org-goto)
         ("s-P"     . anki-editor-push-notes)
         ("s-L"     . org-cliplink)
         ("s-b"     . (lambda () (interactive) (org-emphasize ?*)))
         ("s-/"     . (lambda () (interactive) (org-emphasize ?/)))
         ("s-u"     . (lambda () (interactive) (org-emphasize ?_)))
         ("s-="     . (lambda () (interactive) (org-emphasize ?=)))
         ("s-+"     . (lambda () (interactive) (org-emphasize ?+)))
         ("s-c"     . (lambda () (interactive) (org-emphasize ?~)))
         ("s-s"     . org-subscript-region-or-point)
         ("s-S"     . org-superscript-region-or-point)
         )
  :bind (:map org-mode-map
              ;; Unbinding org-cycle-agenda-files
              ("C-'"          . nil)
              ("C-,"          . nil)
              ("C-c C-v C-g"  . org-babel-goto-named-src-block)
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
  ;; See also org-caldav
  (setq my-private-calendar-directory (expand-file-name "calendar" my-private-conf-directory))
  ;; Personal files
  (setq org-default-notes-file (expand-file-name "plan-office.org" org-directory))
  (setq org-my-plan-free-file (expand-file-name "plan-free.org" org-directory))
  (setq org-my-plan-church-file (expand-file-name "church/plan-church.org" org-directory))
  (setq org-my-office-file (expand-file-name "office.org" org-directory))
  (setq org-my-web-archive-file (expand-file-name "web-archive.org" org-directory))
  (setq org-my-life-file (expand-file-name "life.org" org-directory))
  (setq org-my-beancount-file (expand-file-name "finance/personal.bean" my-sync-directory))
  (setq org-my-anki-file (expand-file-name "anki.org" org-directory))
  (setq org-my-archive-file (expand-file-name "archive.org" org-directory))

  ;; Default org-mode startup
  (setq org-startup-folded t
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-latex-preview-ltxpng-directory (expand-file-name "ltximg/" org-directory))
  ;; Larger latex fragments
  (plist-put org-format-latex-options :scale 1.5)

  ;; Where to archive files
  (setq org-archive-location (concat org-my-archive-file "::* From %s"))

  ;; set todo keywords. As of v9.2.3, any file-local keyword list will overwrite (instead of append) value set in here.
  ;; So actual tags used in Org files are specified using #+SEQ_TODO and #+TYP_TODO instead. Here I keep a complete
  ;; list of tags for font settings
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n)" "IN-PROGRESS(i!)" "WAIT(w@)" "BLOCKED(b@/!)" "SOMEDAY(s)" "CANCELLED(c@/!)" "DONE(d!)")))
  ;; Setup for ordered tasks. Initiate with C-c C-x o
  (setq org-enforce-todo-dependencies nil)
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
   org-agenda-files (list
                     org-directory my-private-calendar-directory
                     (expand-file-name "notes" org-directory)
                     (expand-file-name "projects" org-directory)
                     (expand-file-name "church" org-directory)
                     (expand-file-name "finance" my-sync-directory)))
  ;; Don't include archive files.
  (org-remove-file org-my-web-archive-file)
  (org-remove-file org-my-archive-file)


  (setq
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
     ("w" "Weekly Review"
      ( ;; deadlines
       (tags-todo "+DEADLINE<=\"<today>\""
                  ((org-agenda-overriding-header "Late Deadlines")))
       ;; scheduled  past due
       (tags-todo "+SCHEDULED<=\"<today>\""
                  ((org-agenda-overriding-header "Late Scheduled")))
       ;; now the agenda
       (agenda ""
               ((org-agenda-overriding-header "weekly agenda")
                (org-agenda-ndays 7)
                (org-agenda-tags-todo-honor-ignore-options t)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-deadline-warning-days 0)))
       ;; and last a global todo list
       (todo "TODO")))
     ("o" "Agenda and Office-related Tasks"
      ((agenda "" ((org-agenda-tag-filter-preset '("+office"))
                   ;; Show agenda for this whole week, and first 2 days of next week
                   (org-agenda-start-on-weekday 1) ;; Always start on Monday
                   (org-agenda-span 9))))
      nil ("/tmp/office.html" "/tmp/office.txt" "/tmp/office.pdf" "/tmp/office.ps"))
     ("h" "Home"
      ((agenda "" ((org-agenda-tag-filter-preset '("+home"))
                   ;; Show upcoming 3 days
                   (org-agenda-span 3))))
      nil ("/tmp/home.html" "/tmp/home.txt" "/tmp/home.pdf" "/tmp/home.ps"))
     ("c" "Church"
      ((agenda "" ((org-agenda-tag-filter-preset '("+church"))))))
     ("to" "things TODO in Office"
      ((tags-todo "office/TODO"))
      nil ("/tmp/todo-office.pdf"))
     ("th" "things TODO at Home"
      ((tags-todo "home/TODO"))
      nil ("/tmp/todo-home.pdf"))
     ("tc" "things TODO in Church"
      ((tags-todo "church/TODO"))
      nil ("/tmp/todo-church.pdf"))
     )
   ;; Make it sticky, so it doesn't get killed upon hitting "q". Use "r" to
   ;; refresh instead. Note that it can still be killed by kill-buffer. To
   ;; avoid this, set the emacs-lock-mode
   org-agenda-sticky t
   ;; Don’t show scheduled/deadline/timestamp items in agenda when they are done
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-timestamp-if-done t
   ;; Don't show scheduled/deadlines/timestamp items on todo list.
   org-agenda-todo-ignore-scheduled t
   org-agenda-todo-ignore-deadlines t
   org-agenda-todo-ignore-timestamp t
   org-agenda-todo-ignore-with-date t
   ;; Define when my day really ends (well, normally earlier than that)
   org-extend-today-until 4
   ;; Show meetings in org agenda
   org-agenda-include-diary t)

  (setq org-agenda-exporter-settings
        '((ps-number-of-columns 1)
          (ps-landscape-mode t)
          (org-agenda-add-entry-text-maxlines 5)
          (org-agenda-prefix-format " [ ] ")
          (org-agenda-with-colors t)
          (org-agenda-remove-tags t)
          (htmlize-output-type 'css)))

  ;; Auto save org-files, so that we prevent the locking problem between computers
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  ;; Suppress output "Saving all Org buffers... done"
  (advice-add 'org-save-all-org-buffers :around #'suppress-messages)

  ;; Capturing thoughts and Level 1 Headings.
  (setq org-capture-templates
        '(
          ("a" "Anki basic"
           entry
           (file+headline org-my-anki-file "Dispatch")
           "* %<%H:%M>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n")

          ("A" "Anki cloze"
           entry
           (file+headline org-my-anki-file "Dispatch")
           "* %<%H:%M>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n** Extra\n")

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

  (defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (switch-to-buffer "*scratch*")
    (make-frame '((name . "org-capture") (window-system . x)))
    (select-frame-by-name "org-capture")
    (counsel-org-capture)
    (delete-other-windows)
    )

  ;; Automatically add "CREATED" timestamp to org-capture entries
  ;; See https://emacs.stackexchange.com/questions/21291/add-created-timestamp-to-logbook
  ;; Change: Don't add property when filing at beancount/anki files.
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
                  (string-match "\\.beancount$" (buffer-name))
                  (string-match "\\.bean$" (buffer-name))
                  (string-match "anki.org" (buffer-name)))
        (org-set-property created now))))
  (add-hook 'org-capture-before-finalize-hook #'org-set-created-property)

  ;; General org settings
  (setq-default
   ;; Indentation setting
   ;; Always indent to the left
   org-indent-indentation-per-level 2
   ;; Start up indented
   org-startup-indented 't
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
   ;; allow lists with letters in them.
   org-list-allow-alphabetical t
   ;; Increase imenu index depth
   org-imenu-depth 5
   ;; Interpret sub-superscripts only when they're quoted with braces
   org-use-sub-superscripts '{}
   org-export-with-sub-superscripts '{}
   ;; Do not use babel to evaluate code when exporting.
   org-export-use-babel 't
   ;; Don't include the validation link & creation tag
   org-html-postamble 'nil ;; Don't include validation link and created tags
   ;; Logging settings: Better verbose than miss
   org-log-into-drawer t
   org-log-done 'time
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
        org-habit-graph-column 40
        org-habit-preceding-days 28
        org-habit-following-days 7)

  ;; When clock in a task, don't show Org heading name in mode line
  (defun myorg-remove-clock-in-string ()
    (delete 'org-mode-line-string global-mode-string))
  (add-hook 'org-clock-in-hook 'myorg-remove-clock-in-string)

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
  ;; Enable link to manual pages
  ;; Org-man.el is downloaded from https://orgmode.org/manual/Adding-hyperlink-types.html
  (use-package org-man
    :straight nil)

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
     (lilypond . t)
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
     (asymptote .  t) ;; create vector graphics
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

  ;; Default code block settings
  (setq org-babel-default-header-args:python
        '((:results . "output replace")
          (:session . "none")
          (:exports . "both")
          (:cache .   "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:eval . "never-export")))

  (setq org-babel-default-header-args:R
        '((:results . "output replace")
          (:session . "none")
          (:exports . "both")
          (:cache .   "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:eval . "never-export")))

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
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; This automatically aligns tables, which is nice if you use code to generate
  ;; tables.
  (defun scimax-align-result-table ()
    "Align tables in the subtree."
    (save-restriction
      (save-excursion
        (unless (org-before-first-heading-p) (org-narrow-to-subtree))
        (org-element-map (org-element-parse-buffer) 'table
          (lambda (tbl)
            (goto-char (org-element-property :post-affiliated tbl))
            (org-table-align))))))

  (add-hook 'org-babel-after-execute-hook
            'scimax-align-result-table)

  ;; emphasize commands copied from scimax
  (defun org-markup-region-or-point (type beginning-marker end-marker)
    "Apply the markup TYPE with BEGINNING-MARKER and END-MARKER to region, word or point.
This is a generic function used to apply markups. It is mostly
the same for the markups, but there are some special cases for
subscripts and superscripts."
    (cond
     ;; We have an active region we want to apply
     ((region-active-p)
      (let* ((bounds (list (region-beginning) (region-end)))
             (start (apply 'min bounds))
             (end (apply 'max bounds))
             (lines))
        (unless (memq type '(subscript superscript))
          (save-excursion
            (goto-char start)
            (unless (looking-at " \\|\\<")
              (backward-word)
              (setq start (point)))
            (goto-char end)
            (unless (or (looking-at " \\|\\>")
                        (looking-back "\\>" 1))
              (forward-word)
              (setq end (point)))))
        (setq lines
              (s-join "\n" (mapcar
                            (lambda (s)
                              (if (not (string= (s-trim s) ""))
                                  (concat beginning-marker
                                          (s-trim s)
                                          end-marker)
                                s))
                            (split-string
                             (buffer-substring start end) "\n"))))
        (setf (buffer-substring start end) lines)
        (forward-char (length lines))))
     ;; We are on a word with no region selected
     ((thing-at-point 'word)
      (cond
       ;; beginning of a word
       ((looking-back " " 1)
        (insert beginning-marker)
        (re-search-forward "\\>")
        (insert end-marker))
       ;; end of a word
       ((looking-back "\\>" 1)
        (insert (concat beginning-marker end-marker))
        (backward-char (length end-marker)))
       ;; not at start or end so we just sub/sup the character at point
       ((memq type '(subscript superscript))
        (insert beginning-marker)
        (forward-char (- (length beginning-marker) 1))
        (insert end-marker))
       ;; somewhere else in a word and handled sub/sup. mark up the
       ;; whole word.
       (t
        (re-search-backward "\\<")
        (insert beginning-marker)
        (re-search-forward "\\>")
        (insert end-marker))))
     ;; not at a word or region insert markers and put point between
     ;; them.
     (t
      (insert (concat beginning-marker end-marker))
      (backward-char (length end-marker)))))

  (defun org-subscript-region-or-point ()
    "Mark the region, word or character at point as a subscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
    (interactive)
    (org-markup-region-or-point 'subscript "_{" "}"))

  (defun org-superscript-region-or-point ()
    "Mark the region, word or character at point as superscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
    (interactive)
    (org-markup-region-or-point 'superscript "^{" "}"))

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
  (setq-default org-download-method 'attach ;; Screenshots are stored in data/ directory by ID. Easier to manage
                org-download-heading-lvl nil
                org-download-delete-image-after-download t
                org-download-screenshot-method "echo"
                org-download-screenshot-file "/tmp/screenshot.png"
                org-download-image-org-width 800
                org-download-annotate-function (lambda (link) "") ;; Don't annotate
                )

  ;; My customized org-download to incorporate flameshot gui Workaround to setup flameshot, which enables annotation.
  ;; In flameshot, set filename as "screenshot", and the command as "flameshot gui -p /tmp", so that we always ends up
  ;; with /tmp/screenshot.png. Nullify org-download-screenshot-method by setting it to `echo', so that essentially we
  ;; are only calling (org-download-image org-download-screenshot-file).
  (defun my-org-download-screenshot ()
    "Capture screenshot and insert the resulting file.
The screenshot tool is determined by `org-download-screenshot-method'."
    (interactive)
    (let ((tmp-file "/tmp/screenshot.png"))
      (delete-file tmp-file)
      (call-process-shell-command "flameshot gui -p /tmp/")
      ;; Because flameshot exit immediately, keep polling to check file existence
      (while (not (file-exists-p tmp-file))
        (sleep-for 2))
      (org-download-image tmp-file)))
  (global-set-key (kbd "<print>") 'my-org-download-screenshot)
  ;; Use #+ATTR_ORG: :width 300px to customized image display width
  (setq org-image-actual-width nil)

  (use-package org-recent-headings
    :defer 3
    :config (org-recent-headings-mode))

  ;; org-chef
  (require 'org-chef)

  ;; org-crypt
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key "")

  ;; org-journal
  (use-package org-journal
    :after org
    :defer 3
    :bind (("C-c J" . org-journal-new-entry))
    :custom
    (org-journal-dir (expand-file-name "journal/" org-directory))
    (org-journal-date-format "%A, %d %B %Y")
    (org-journal-time-format "%H:%M")
    (org-journal-enable-agenda-integration t)
    (org-journal-file-type 'daily)
    (org-journal-tag-alist '(("idea" . ?i) ("schedule" . ?i) ("spirituality" . ?s)))
    (org-journal-time-prefix "** ")
    (org-journal-encrypt-journal t)
    (org-journal-enable-encryption nil))

  ;; org-contacts
  (use-package org-contacts
    :defer 10
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
    :straight biblio ;; Browse and import bibliographic references from CrossRef, DBLP, HAL, arXiv, Dissemin, and doi.org
    :bind ("H-b" . ivy-bibtex) ;; open bibliography
    :hook ((org-noter-notes-mode org-noter-doc-mode) . hide-mode-line-mode) ;; Hide modeline when taking notes
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
    (setq-default org-noter-notes-search-path `(,(expand-file-name "notes" org-directory))
                  org-noter-default-notes-file-names '("paper-notes.org")
                  org-noter-auto-save-last-location t
                  org-noter-always-create-frame t)

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
  (setq org-super-agenda-groups
        '((:name "Schedule"
                 :time-grid t)
          (:name "Due today"
                 :deadline today)
          (:name "Today"
                 :and (:scheduled today :not (:habit t))) ;; Show habits separately.
          (:name "Overdue"
                 :deadline past)
          (:name "Habits"
                 :habit t)
          (:name "Due soon"
                 :deadline future)
          (:name "Scheduled earlier"
                 :scheduled past)
          ))
  (org-super-agenda-mode)

  ;; Org-icalendar setting
  (setq-default
   ;; Please make sure to set your correct timezone here
   org-icalendar-timezone "Asia/Hong_Kong"
   org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S"
   ;; Alarm me 15 minutes in advance
   org-icalendar-alarm-time 15
   ;; This makes sure to-do items as a category can show up on the calendar
   org-icalendar-include-todo t
   ;; ensures all org "deadlines" show up, and show up as due dates
   org-icalendar-use-deadline '(event-if-todo todo-due event-if-todo-not-done)
   ;; ensures "scheduled" org items show up, and show up as start times
   org-icalendar-use-scheduled '(event-if-todo todo-start event-if-todo-not-done)
   )

  ;; helm-org-rifle
  (setq helm-org-rifle-show-path t)

  ;; Export to Confluence Wiki
  (require 'ox-confluence)

  ;; Enable KOMA-script support
  (require 'ox-koma-letter)

  ;; Export to mermaid
  ;; npm install mermaid.cli
  (setq ob-mermaid-cli-path "/home/yiufung/node_modules/.bin/mmdc")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)))

  ;; org-pdftools
  (use-package org-pdftools
    :straight (:host github :repo "fuxialexander/org-pdftools")
    :defer 3)

  ;; Use org-bookmark-heading
  (use-package org-bookmark-heading
    :defer 3)

  ;; Org-Msg mode. Send email the Outlook style
  (require 'org-msg)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi %s,\n\n"
        org-msg-greeting-fmt-mailto t
        org-msg-signature "
Regards,
#+begin_signature
Yiufung
#+end_signature")
  ;; org-msg doesn't support notmuch for now.
  (setq mail-user-agent 'message-user-agent)
  (defalias 'html-mail-mode 'org-msg-mode) ;; An easy-to-remember name
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
;; Reading email: notmuch-emacs / gnus
;; Sending email: msmtp/msmtp-mta

;; General mail settings
(setq-default
 ;; Sendmail is an alias to msmtp after installing msmtp and msmtp-mta
 send-mail-function 'sendmail-send-it
 message-send-mail-function 'message-send-mail-with-sendmail
 ;; Three variables to work with msmtp to enable multiple accounts
 mail-specify-envelope-from 't
 message-sendmail-envelope-from 'header
 mail-envelope-from 'header)

(use-package gnus
  :bind (("C-c m" . 'gnus))
  :bind (:map gnus-article-mode-map
              ("o" . gnus-mime-copy-part)
              :map gnus-topic-mode-map
              ("<tab>" . gnus-topic-select-group))
  :hook
  (gnus-select-group-hook . gnus-group-set-timestamp)
  (gnus-summary-exit-hook . gnus-topic-sort-groups-by-alphabet)
  (gnus-summary-exit-hook . gnus-group-sort-groups-by-rank)
  (gnus-group-mode . gnus-topic-mode)
  ((gnus-browse-mode gnus-server-mode gnus-group-mode gnus-summary-mode) . hl-line-mode)
  (dired-mode . gnus-dired-mode)
  :config
  (require 'gnus-dired)
  (require 'gnus-topic)

  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods
        '((nntp "gmane" (nntp-address "news.gmane.io"))
          (nntp "news.gwene.org")
          ;; Still use mbsync to sync Mail directory
          (nnmaildir "church"
                     (directory "~/Maildir/church/"))
          (nnmaildir "fastmail"
                     (directory "~/Maildir/fastmail/"))))

  ;; Render HTML content using gnus-w3m
  (setq mm-text-html-renderer 'gnus-w3m)

  (setq gnus-inhibit-images nil ;; Keep images displayed
        )

  (setq nnmail-expiry-wait 30)
  (setq mm-encrypt-option 'guided)
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t)

  ;; gnus article
  (setq gnus-article-browse-delete-temp 'ask)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-number)
          (not gnus-article-sort-by-date)))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-max-image-proportion 0.3)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^To:" "^Cc:" "^Newsgroups:" "^Subject:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)

  ;; Gnus group
  (setq gnus-level-subscribed 6)
  (setq gnus-level-unsubscribed 7)
  (setq gnus-level-zombie 8)
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-mode-line-format "%%b")

  ;; Gnus topic
  (setq gnus-topic-display-empty-topics nil)

  ;; gnus summary
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-date)
          (not gnus-thread-sort-by-number)))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject nil)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))
  (setq gnus-summary-line-format "%U%R%z %-16,16&user-date;  %4L:%-30,30f  %B%S\n")
  (setq gnus-summary-mode-line-format "%p")
  (setq gnus-sum-thread-tree-false-root "─┬➤ ")
  (setq gnus-sum-thread-tree-indent " ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-single-leaf "└─➤ ")
  (setq gnus-sum-thread-tree-vertical "│")
  )

(use-package notmuch
  ;; Another mail client. It's better at email searching
  :disabled
  :defer 3
  :straight nil
  :straight counsel-notmuch
  :straight nm
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
   ;; Show html where possible
   notmuch-multipart/alternative-discouraged '("text/plain" "text/html")
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

  ;; Experimental front end Nevermore
  (require 'nm-company)
  )

(use-package outlook
  :straight nil
  :bind ("C-x M-e" . export-org-email)
  :defer 3
  :config
  ;; Writing Outlook-style email in Org-mode
  ;; See https://coredumped.dev/2019/02/08/using-org-mode-to-write-email-for-outlook/
  (defun org-email-html-head ()
    "Create the header with CSS for use with email"
    (concat
     "<style type=\"text/css\">\n"
     "<!--/*--><![CDATA[/*><!--*/\n"
     (with-temp-buffer
       (insert-file-contents
        (concat my-emacs-conf-directory "static/outlook.css"))
       (buffer-string))
     "/*]]>*/-->\n"
     "</style>\n"))

  (defun export-org-email ()
    "Export the current email org buffer and copy it to the clipboard"
    (interactive)
    (let ((org-export-show-temporary-export-buffer nil)
          (org-html-head (org-email-html-head))
          (org-export-with-author nil) ;; Remove user and TOC
          (org-export-with-toc nil)
          (org-export-headline-levels 2) ;; Export up to 2 headlines
          (org-export-with-section-numbers t)
          (org-html-postamble nil)) ;; Don't include validation link and created tags
      (org-html-export-as-html nil t nil nil nil) ;; Only export the current subtree
      (with-current-buffer "*Org HTML Export*"
        (kill-new (buffer-string)))
      (message "HTML copied to clipboard")))
  )

(use-package org-caldav
  ;; Fastmail Calendar integration
  :defer 3
  :after org
  :config
  (setq
   ;; The CalDAV URL with your full and primary email address at the end.
   org-caldav-url "https://caldav.fastmail.com/dav/calendars/user/mail@yiufung.net"
   ;; Only entries with "schedule" tags should be exported to CalDAV
   org-caldav-select-tags '("schedule")
   ;; Multiple calendar setup
   org-caldav-calendars `(
                          (:calendar-id "1fe12417-fe19-41d2-a105-a94d1a562e21"
                                        :files (,org-my-plan-free-file ,org-my-life-file)
                                        :inbox ,(expand-file-name "CalHome.org" my-private-calendar-directory))
                          (:calendar-id "d556f213-2b71-4bcd-a1a4-370a9b1a1eae"
                                        :files (,org-my-office-file ,org-default-notes-file) ;; default note is plan-office
                                        :inbox ,(expand-file-name "CalOffice.org" my-private-calendar-directory))
                          (:calendar-id "7100372b-7ab5-409f-a68e-8977c19e77bf"
                                        :files (,org-my-plan-church-file)
                                        :inbox ,(expand-file-name "CalChurch.org" my-private-calendar-directory))
                          )
   ;; If entries are deleted in Org, always delete at the CALDAV end without asking
   org-caldav-delete-calendar-entries 'always
   ;; Never delete at local Org side
   org-caldav-delete-org-entries 'never
   ;; Never resume aborted sync
   org-caldav-resume-aborted 'never
   ;; Debug like crazy
   org-caldav-debug-level 2
   ;; Change org-caldav save directory
   org-caldav-save-directory my-private-calendar-directory
   )

  (defvar org-caldav-sync-timer nil
    "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
  (defun org-caldav-sync-with-delay (mins)
    (when org-caldav-sync-timer
      (cancel-timer org-caldav-sync-timer))
    (setq org-caldav-sync-timer
          (run-with-idle-timer
           (* 60 mins) nil 'org-caldav-sync)))
  ;; Add the delayed save hook with a half-hour idle timer
  (add-hook 'after-save-hook (lambda () (org-caldav-sync-with-delay 30)))
  )

;;; Contacts: bbdb

(use-package bbdb
  :commands bbdb
  :config
  (setq bbdb-file (expand-file-name "bbdb" my-private-conf-directory))
  (bbdb-initialize 'message 'anniv)
  (bbdb-mua-auto-update-init 'message)
  (setq bbdb-mua-pop-up-window-size 10)

  ;; Make sure we look at every address in a message and not only the first one
  (setq bbdb-message-all-addresses t)
  )
;;; Remote Editing: Tramp

(use-package tramp
  ;; Remote editing
  :defer 5
  :straight nil
  :straight counsel-tramp
  :bind ("C-c t" . counsel-tramp)
  :config
  (setq
   ;; Allow loading .dir-locals.el in remote.
   ;; Might be slower but very useful when code base is in remote.
   enable-remote-dir-locals t
   explicit-shell-file-name "/bin/bash" ;; Set default shell
   tramp-verbose 1
   tramp-default-method "ssh"
   tramp-auto-save-directory "~/.cache/emacs/backups"
   vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

  ;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
  (push
   (cons
    "docker"
    '((tramp-login-program "docker")
      (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods)

  (defadvice tramp-completion-handle-file-name-all-completions
      (around dotemacs-completion-docker activate)
    "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
    (if (equal (ad-get-arg 1) "/docker:")
        (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
               (dockernames (cl-remove-if-not
                             #'(lambda (dockerline) (string-match ":$" dockerline))
                             (split-string dockernames-raw "\n"))))
          (setq ad-return-value dockernames))
      ad-do-it))
  )

(use-package ssh-config-mode
  :bind (:map ssh-config-mode-map
              ("C-c C-p" . ssh-config-host-prev)
              ("C-c C-n" . ssh-config-host-next)))

;;; View Documents: Info / DocView / PDF-Tools / Nov.el

(use-package info
  :straight nil
  :config
  ;; Run make info to compile info documentation
  (setq Info-additional-directory-list `(,(expand-file-name "straight/repos/org/doc/" user-emacs-directory))))

(use-package browse-url
  :straight nil
  :defer 3
  :config
  ;; Open url in Firefox by default
  (setq browse-url-browser-function 'browse-url-firefox))

(use-package doc-view
  ;; Requires unoconv, ghostscript, dvipdf
  :custom (doc-view-odf->pdf-converter-program "soffice"))

(use-package pdf-tools
  :defer t
  ;; :pin manual ;; manually update
  :straight tablist
  :straight hydra
  :straight web-server
  :load-path (lambda () (if (memq system-type '(windows-nt)) ;; If under Windows, use the customed build in Dropbox.
                        (expand-file-name "elisp/pdf-tools-20180428.827/"
                                          my-emacs-conf-directory)))
  ;; Tell Emacs to autoloads the package
  ;; :init (load "pdf-tools-autoloads" nil t)
  ;; If under Linux, manually install it with package-install.
  ;; If there's error for pdf-occur mode, delete pdf-occur.elc manually.
  :bind (:map pdf-view-mode-map
              ("C-s" . 'isearch-forward)
              ("C-r" . 'isearch-backward)
              ("C-z p" . hydra-pdftools/body)
              ("M-w" . 'pdf-view-kill-ring-save)
              ("h" . 'pdf-annot-add-highlight-markup-annotation)
              ("t" . 'pdf-annot-add-text-annotation)
              ("D" . 'pdf-annot-delete)
              ("S" . 'sync-pdf-in-pdfjs)
              )
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-height)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotation t)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)

  (pdf-tools-install t) ;; Install pdf tools with no queries

  ;; Start a CORS-enabled web-server from within Emacs, so that PDF.js can synchronize with pdf-tools
  (lexical-let ((docroot (expand-file-name "portable-ebooks" my-sync-directory)))
    (ws-start
     (lambda (request)
       (with-slots (process headers) request
         (let ((path (substring (cdr (assoc :GET headers)) 1)))
           (if (ws-in-directory-p docroot path)
               (if (file-directory-p path)
                   (ws-send-directory-list process
                                           (expand-file-name path docroot) "^[^\.]")
                 (ws-response-header process 200 '("Access-Control-Allow-Origin" . "*"))
                 (ws-send-file process (expand-file-name path docroot)))
             (ws-send-404 process)))))
     9005 nil :name (format "pdfjs-%s" docroot)))

  ;; In Arch Linux, need to install pdfjs package first.
  (defun sync-pdf-in-pdfjs (&optional file)
    "Open current PDF in the corresponding page in PDF.js."
    (interactive)
    (or file
        (setq file (buffer-name))
        (error "Current buffer has no file"))
    (let ((browse-url-browser-function 'browse-url-firefox)
          (port 9005)) ;; Should match to CORS-enabled server that points to PDF directory
      (browse-url (format "%s?file=%s#page=%d"
                          "file:///usr/share/pdf.js/web/viewer.html"
                          (format "http://localhost:%d/%s" port (url-hexify-string file))
                          (pdf-view-current-page)))
      (run-hooks 'browse-url-of-file-hook)))

  (defun open-text-in-firefox (beg end)
    "Export selected region as HTML, and open it in Firefox.

Useful for utilizing some plugins in Firefox (e.g: to make Anki cards)"
    (interactive "r")
    (copy-region-as-kill beg end)
    (let ((tmpfile (make-temp-file "cyf-text" nil ".html")))
      (with-temp-file tmpfile
        (yank)
        (mark-whole-buffer)
        (org-html-convert-region-to-html)
        (browse-url-firefox (concat "file://" tmpfile))))
    )

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
  :bind (:map nov-mode-map
              ("n" . 'scroll-down-command)
              ("p" . 'scroll-up-command))
  :hook (nov-mode . (lambda () (progn (visual-fill-column-mode) (setq visual-fill-column-width 80)))) ;; Easier to read.
  :hook (nov-mode . (lambda () (beacon-mode -1))) ;; Don't show beamer.
  :config
  (setq nov-text-width 'nil)
  ;; (require 'justify-kp)
  ;; (setq nov-text-width t)

  ;; (defun my-nov-window-configuration-change-hook ()
  ;;   (my-nov-post-html-render-hook)
  ;;   (remove-hook 'window-configuration-change-hook
  ;;                'my-nov-window-configuration-change-hook
  ;;                t))

  ;; (defun my-nov-post-html-render-hook ()
  ;;   (if (get-buffer-window)
  ;;       (let ((max-width (pj-line-width))
  ;;             buffer-read-only)
  ;;         (save-excursion
  ;;           (goto-char (point-min))
  ;;           (while (not (eobp))
  ;;             (when (not (looking-at "^[[:space:]]*$"))
  ;;               (goto-char (line-end-position))
  ;;               (when (> (shr-pixel-column) max-width)
  ;;                 (goto-char (line-beginning-position))
  ;;                 (pj-justify)))
  ;;             (forward-line 1))))
  ;;     (add-hook 'window-configuration-change-hook
  ;;               'my-nov-window-configuration-change-hook
  ;;               nil t)))

  ;; (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)
  )

;;; Spell-checking / Dictionary Lookup / Chinese input

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
                                        "Roget's II The New Thesaurus 3th Ed. (En-En)"
                                        "Webster's Revised Unabridged Dictionary (1913)"
                                        "牛津高阶英汉双解"
                                        "Oxford Advanced Learner's Dictionary"
                                        "牛津现代英汉双解词典"
                                        "懒虫简明英汉词典"
                                        "jmdict-ja-en"
                                        "WordNet"
                                        )
        sdcv-tooltip-border-width 5
        sdcv-tooltip-timeout 5
        sdcv-dictionary-data-dir (expand-file-name "stardict" my-private-conf-directory))
  )

(use-package mw-thesaurus
  :bind (("C-c T" . mw-thesaurus-lookup-at-point))
  )

(use-package pyim
  :demand t
  :straight t
  :straight pyim-basedict
  ;; Need to run "make liberime-core" on build directory.
  :straight (liberime
             :host github
             :repo "merrickluo/liberime"
             :files ("CMakeLists.txt" "Makefile" "src" "liberime.el" "liberime-config.el"))
  :init
  ;; See https://github.com/rime/home/wiki/UserGuide#%E5%90%8C%E6%AD%A5%E7%94%A8%E6%88%B6%E8%B3%87%E6%96%99
  ;; For syncing rime dictionary across laptops.
  (setq pyim-title "ㄓ")
  (add-hook 'liberime-after-start-hook
            (lambda ()
              (liberime-select-schema "luna_pinyin")))
  :bind (("M-j" . pyim-convert-string-at-point)  ;与 pyim-probe-dynamic-english 配合
         :map pyim-mode-map
         ("." . pyim-page-next-page)
         ("," . pyim-page-previous-page))
  :config
  (require 'liberime)
  ;; 儘可能試用 posframe 彈出
  (setq default-input-method "pyim"
        pyim-default-scheme 'rime
        ;; 选词框显示9个候选词。
        pyim-page-length 9)
  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标前是汉字字符时，才能输入中文。
  ;; 2. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-org-speed-commands
                  pyim-probe-org-structure-template
                  pyim-probe-org-latex-mode))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; Reset some punctuation mappings.
  (delete '("/" "、")  pyim-punctuation-dict)
  (add-to-list 'pyim-punctuation-dict '("\\" "、"))
  (delete '("}" "』")  pyim-punctuation-dict)
  (delete '("{" "『")  pyim-punctuation-dict)
  (add-to-list 'pyim-punctuation-dict '("{" "「"))
  (add-to-list 'pyim-punctuation-dict '("}" "」"))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  (defun liberime-change-schema ()
    "Change liberime input Schema"
    (interactive)
    (liberime-select-schema
     (ivy-read "Input schema: "
               (liberime-get-schema-list)
               :require-match t)))
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

(use-package so-long
  ;; TODO: Going to be included in Emacs 27.1
  ;; Avoid performance issues in files with very long lines.
  :straight nil
  :defer 3
  :config
  (setq so-long-threshold 1000)
  (global-so-long-mode 1)
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
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'python-mode)
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

;;;; Terminal Support

(use-package vterm
  :defer 3
  ;; Don't let whole-line-or-region shadows the C-y
  :config
  (defun create-or-switch-to-vterm ()
    "Switch to default `vterm' buffer.
      Start `vterm' if it's not already running."
    (interactive)
    (pop-to-buffer "vterm" nil t)
    (if (not (equal major-mode 'vterm-mode))
        (vterm-mode)))
  :hook (vterm-mode . (lambda () (whole-line-or-region-local-mode -1)))
  :bind (("C-z C-z" . create-or-switch-to-vterm)
         :map vterm-mode-map
         ("C-y"  . vterm-yank)
         ("<f5>" . nil)
         ("<f6>" . nil)
         ("<f7>" . nil)
         ("<f8>" . nil)
         ("<f9>" . nil)
         ("<f10>" . nil)
         ("<f11>" . nil)
         ("<f12>" . nil)))

(use-package shell
  :straight nil
  :straight dirtrack
  :straight ssh
  :defer 3
  :bind ("C-z C-j" . shell) ;; Bind to j to mimic "jump to"
  :hook (shell-mode-hook . visual-line-mode)
  :config
  (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)]" 1 nil))
  (dirtrack-mode +1)
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
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 1
        company-idle-delay 0.5)

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
  (company-quickhelp-mode +1)
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
                                           try-expand-dabbrev-visible
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
  :hook (prog-mode . hs-minor-mode)
  :init
  (defun toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)))
  :bind (:map prog-mode-map
              ("C-c h" . toggle-fold)))

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
  :straight cdlatex
  :hook (LaTeX-mode . magic-latex-buffer) ; Auto-enable fancy display of symbols.
  :hook (LaTeX-mode . turn-on-cdlatex) ; fast insertion of environment templates and math stuff
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
  :defer 3
  :bind (("C-c y" . 'yas-insert-snippet))
  :config
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "yasnippets" my-private-conf-directory))
  (yas-global-mode 1)

  ;; Enable yasnippet expansion in minibuffer
  ;; https://emacs.stackexchange.com/questions/36677/how-to-use-yasnippets-tab-expansion-in-minibuffer
  (add-hook 'minibuffer-setup-hook 'yas-minor-mode)
  (define-key minibuffer-local-map [tab] yas-maybe-expand)

  (yasnippet-snippets-initialize)
  )

;;;; Syntax Checking: Flycheck

(use-package flycheck
  :defer 5
  :straight hydra
  :straight posframe
  :straight flycheck-posframe
  :bind ("C-z !" . hydra-flycheck/body)
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
  :straight lsp-ivy
  :straight lsp-treemacs
  :defer 3
  :after company
  :hook (python-mode . lsp) ;; Start LSP server in python-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-enable-snippet nil
        company-lsp-enable-snippet t)
  )

;;;; Debugging

(use-package dap-mode
  :defer 3
  :config
  (require 'dap-python))

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
  :if (equal system-type 'gnu/linux)
  :defer 5
  :after org
  :config
  ;; ob-jupyter integration is added in org settings.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jupyter . t)))
  )

(use-package ein
  ;; Jupyter Notebook in Emacs
  :straight t
  :straight (simple-httpd :host github :repo "skeeto/emacs-web-server" :local-repo "simple-httpd")
  :bind (("H-j" . 'ein:notebooklist-login)
         ("H-h" . 'ein:jupyterhub-connect)
         ("H-J" . 'ein:jupyter-server-start)
         :map ein:notebook-mode-map
         ("s-n" . ein:worksheet-goto-next-input)
         ("s-p" . ein:worksheet-goto-prev-input)
         ("s-'" . ein:worksheet-turn-on-autoexec))
  :commands (ein:notebooklist-login ein:jupyter-server-start ein:jupyterhub-connect)
  :hook (ein:notebook-mode . visual-line-mode)
  :init
  ;; Need to require here to initialize 'ein:notebook-mode-map, so that :bind directive works.
  (require 'ein-notebook)
  :config
  (setq-default ein:worksheet-enable-undo 't
                ein:polymode 'nil)
  (add-to-list 'ein:notebook-mode-hook '(lambda () (show-paren-mode -1)))
  )

;;;; Python
(setq-default
 ;; Don't warn me when guessing indent
 python-indent-guess-indent-offset-verbose nil
 ;; Don't enable native readline completion
 python-shell-completion-native-enable nil)

(use-package python
  :straight nil
  :preface
  (defun create-or-switch-to-python ()
    "Switch to default `python' buffer.
      Start `python' if it's not already running."
    (interactive)
    (pop-to-buffer "*Python*" nil t)
    (if (not (equal major-mode 'inferior-python-mode))
        (run-python)))
  :bind ("C-z C-p" . create-or-switch-to-python))

(use-package elpy
  :hook (python-mode . turn-off-auto-fill)
  :disabled
  :config
  (elpy-enable)
  )

(use-package pyvenv
  :defer 3)

(use-package blacken
  ;; Reformat python buffers using the "black" formatter
  :config
  (blacken-mode 1)
  )

;;;; R

(use-package ess-r-mode
  :defer 5
  :straight ess
  ;; NOTE: When using with flycheck-lintr-caching = t (default), make sure
  ;; ~/.R/lintr_cache directory is created.
  :hook (ess-r-mode . turn-off-auto-fill)
  :hook (inferior-ess-r-mode . visual-line-mode)
  :bind (("C-z C-r" . R)
         :map ess-r-mode-map
         ("<C-return>" . nil)
         ("C-c <C-return>" . ess-eval-region-or-line-visibly-and-step))
  :config
  (setq  ess-default-style 'RStudio ;; Default code style: RStudio
         ess-tab-complete-in-script t ;; Tries to complete in script buffers
         ess-eldoc-show-on-symbol 'nil
         ess-eval-visibly 'nowait ;; whether ess-eval-* commands display commands in process buffer
         ess-indent-with-fancy-comments 'nil  ;; Don't distinguish between #, ## and ###
         ess-smart-S-assign-key 'nil ;; Disabled auto replace of "_" to "<-"
         ess-directory 'nil ;; By default starts ESS at current buffer default directory
         ess-ask-for-ess-directory nil
         ess-user-full-name "Cheong Yiu Fung"
         )

  ;; R-specific settings: Add a general summary function
  ;; (add-to-list ess-r-describe-object-at-point-commands)
  (defun cyf/ess-style-current-file ()
    (interactive)
    (if (string= ess-dialect "R")
        (ess-eval-linewise (concat "styler::style_file(\"" buffer-file-name "\")"))))
  (defun cyf/ess-style-current-dir ()
    (interactive)
    (if (string= ess-dialect "R")
        (ess-eval-linewise (concat "styler::style_dir(\"" default-directory "\")"))))
  )

;;;; Lisp

(use-package elisp-mode
  :straight nil
  :bind (("C-z C-l" . ielm) ;; Lisp
         :map emacs-lisp-mode-map
         ("C-c C-c" . eval-defun)
         ("C-c C-b" . eval-buffer)))

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
  ;; Strict modes
  (--each '(css-mode-hook
            restclient-mode-hook
            js-mode-hook
            java-mode
            emacs-lisp-mode-hook
            ielm-mode-hook
            ruby-mode
            markdown-mode
            groovy-mode
            scala-mode)
    (add-hook it 'turn-on-smartparens-strict-mode))
  ;; Non strict modes
  (--each '(ess-mode-hook
            inferior-ess-r-mode-hook)
    (add-hook it 'smartparens-mode))
  )

;;;; HTML

(use-package impatient-mode)

;;;; Clojure

(use-package cider
  :straight t)

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

;; TODO: Ensime is no longer being developed. Replace with something else.

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
  ;; :straight vmd-mode ; For GFM preview
  :straight edit-indirect
  :straight grip-mode
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
  (setq-default markdown-enable-math t
                markdown-asymmetric-header t
                markdown-hide-urls t)
  )

(use-package polymode
  :straight poly-markdown ; RMarkdown support
  :straight poly-R
  :after markdown-mode
  :mode ("\\.md" . poly-markdown-mode)
  :mode ("\\.Rmd\\'" . poly-markdown+r-mode)
  :bind (:map markdown-mode-map
              ("s-n" . polymode-next-chunk)
              ("s-p" . polymode-previous-chunk)
              ("s-N" . polymode-next-chunk-same-type)
              ("s-P" . polymode-previous-chunk-same-type)
              )
  )

;;;; HTTP

(use-package restclient
  :straight t
  :straight ob-restclient
  :straight company-restclient
  :mode ("\\.http" . restclient-mode)
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

(use-package wakatime-mode
  ;; Programming statistics
  :defer 3
  :disabled
  :config
  (setq wakatime-api-key (auth-source-pass-get "api" "wakatime"))
  (global-wakatime-mode)
  )
(use-package ahk-mode
  :mode "\\.ahk\\'")
(use-package json-mode
  ;; JavaScript
  :mode ("\\.json\\'" . json-mode)
  :bind (("s-J" . json-mode-beautify))
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
  :defer 3
  )
(use-package csv-mode
  ;; CSV
  )
(use-package yaml-mode)

;;; Code Highlighting/Searching/Replacing

(use-package highlight-indentation
  :disabled
  :init
  (require 'highlight-indentation)
  :custom-face (highlight-indentation-face ((t (:background "#e3e3d3"))))
  :custom-face (highlight-indentation-current-column-face ((t (:background "#c3b3b3"))))
  :hook (python-mode . highlight-indentation-mode)
  ;; :hook (python-mode . highlight-indentation-current-column-mode)
  ;; (defun set-hl-indent-color ()
  ;;   (set-face-background 'highlight-indentation-face "#e3e3d3")
  ;;   (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))
  ;; (add-hook 'python-mode-hook 'highlight-indentation-mode)
  ;; (add-hook 'python-mode-hook 'set-hl-indent-color)
  )

(use-package color-moccur
  ;; :commands (moccur)
  ;; TODO
  :disabled
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
  :bind (("C-s s" . rg-dwim)))

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
(global-set-key (kbd "<f7>") 'rot13-mode)
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
(use-package base16-theme)
(use-package parchment-theme)
(use-package gruvbox-theme)
(use-package one-themes)
(use-package naysayer-theme)
(use-package modus-operandi-theme)
(use-package modus-vivendi-theme)
(use-package doom-themes)

;; (load-theme 'modus-operandi t)
;; (load-theme 'modus-vivendi t)

;; (load-theme 'kaolin-light)
;; (load-theme 'doom-opera-light t)

(message "my-solaire-themes")
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
(use-package my-fonts
  :no-require t
  :straight nil
  :demand t
  :config
  (defun cyf/set-fonts ()
    "Personal font settings."
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
  )

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

(defalias 'rot13-mode 'toggle-rot13-mode)

;; Viewing Image in Emacs
(use-package image-mode
  :straight nil
  :bind (:map image-mode-map
              ("H" . image-transform-fit-to-height)
              ("P" . image-transform-fit-to-width))
  :hook (image-mode . hide-mode-line-mode)
  :hook (image-mode . image-transform-fit-to-height)
  )

(use-package keycast)

;; Emacs Application Framework
;; https://github.com/manateelazycat/emacs-application-framework
(use-package eaf
  :disabled
  :straight (:host github :repo "manateelazycat/emacs-application-framework"))

(use-package olivetti
  ;; Center text for nicer writing and reading
  :defer 3
  ;; :hook (org-mode . olivetti-mode)
  ;; :hook (org-agenda-mode . olivetti-mode)
  :config
  (setq-default olivetti-body-width 0.9)
  )

(use-package treemacs
  :commands treemacs)

(use-package anki-editor
  :defer 10
  :after org
  :bind (:map org-mode-map
              ("<f12>" . anki-editor-cloze-region-dont-incr)
              ("<f11>" . anki-editor-cloze-region-auto-incr)
              ("<f10>" . anki-editor-reset-cloze-number)
              ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  )

(use-package howdoyou
  :defer 10)

;; Need to pip3 install --user my_cookies, and login leetcode
(use-package leetcode
  :defer 10
  :config
  (setq leetcode-prefer-language "python3"
        leetcode-prefer-sql "oraclesql"))

(use-package dtk
  ;; Bible Study
  :defer 3
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

(use-package wiki-summary
  :defer 5
  :config
  (setq-default wiki-summary-language-string "zh")
  )

(use-package alert
  ;; Notification
  :config
  (setq alert-default-style 'libnotify))

(use-package lorem-ipsum
  ;; Random text generator
  :defer 3)

(use-package rfc
  ;; Read RFC documentation
  :defer 10
  :config
  (setq rfc-mode-directory (expand-file-name "~/projects/rfc/")))

(use-package calfw
  ;; Prettier calendar
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
  ;; Sync office365 calendar
  :disabled
  :defer 3
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
                excorporate-calendar-show-day-function 'exco-org-show-day)

  ;; Make sure that Emacs diary knows how to follow `#include "..."'
  ;; directives (needed by excorporate)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  (excorporate-diary-enable)
  )

(use-package telega)

(use-package operate-on-number
  :bind ("C-c N" . operate-on-number-at-point))

(use-package elfeed
  ;; RSS Reader
  :defer 3
  :straight t
  :straight elfeed-org
  :straight elfeed-goodies
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
  (elfeed-goodies/setup)
  ;; Remove strange choice of keymap in elfeed-goodies
  (define-key elfeed-show-mode-map (kbd "M-v") nil)
  )

(use-package docker
  :straight dockerfile-mode
  :bind ("H-d" . docker))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package keyfreq
  ;; Track key frequencies
  :defer 10
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package beancount
  ;; Manage personal finance
  :straight nil
  :mode (("\\.beancount\\'" . beancount-mode)
         ("\\.bean\\'"      . beancount-mode))
  :hook (beancount-mode . turn-off-auto-fill)
  ;; We only want indentation in org-mode but not beancount-mode
  :hook (beancount-mode . (lambda () (org-indent-mode -1)))
  :bind (:map beancount-mode-map
              ("C-c p") . nil)
  :defer 3
  :config
  (setq beancount-use-ido 'nil)
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
  :hook (erc-mode . visual-line-mode)
  :config
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#archlinux" "#bash" "#emacs" "#latex" "#org-mode" "#python" "#xmonad" "#r" "#anki"))
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
  :bind (("C-c S" . prodigy))
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

;; Code sharing service
(require 'ixio)
(defalias 'share-code-snippet 'ixio-paste)

;;; Start Emacs Server
(load-theme 'kaolin-light t)

(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
