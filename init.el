;;; init.el --- Organized Emacs configuration file

;;; Commentary:
;;; This is a comprehensive Emacs configuration with evil-mode, project management,
;;; and various productivity enhancements.

;;; Code:

;; ============================================================================
;; SECTION 1: BASIC EMACS SETTINGS AND APPEARANCE
;; ============================================================================

;; Disable UI elements for a cleaner look
(menu-bar-mode -1)                  ; Disable menu bar
(tool-bar-mode -1)                  ; Disable toolbar
(scroll-bar-mode -1)                ; Disable scroll bars
(setq inhibit-startup-screen 1)     ; Disable startup screen

;; Buffer and editing preferences
(setq-default fill-column 80)       ; Set line width to 80 characters
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ; Show column indicator
(display-fill-column-indicator-mode) ; Enable the column indicator globally
(setq my-line-indicator-color "misty rose")
(set-face-background 'fill-column-indicator my-line-indicator-color)
(set-face-foreground 'fill-column-indicator my-line-indicator-color)

;; Whitespace handling
(setq tab-width 4)                  ; Set tab width to 4 spaces
(setq indent-tabs-mode nil)         ; Use spaces instead of tabs
(setq mode-require-final-newline t) ; Ensure files end with a newline
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Remove trailing whitespace on save

;; UI feedback settings
(setq echo-keystrokes .1)           ; Show keys being pressed quickly
(setq ring-bell-function 'ignore)   ; Disable the bell/beep

;; Backup and auto-save configuration
(unless (file-directory-p "~/.emacs.d/autosaves")
  (make-directory "~/.emacs.d/autosaves"))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups/")))
(setq create-lockfiles nil)         ; Don't create lock files

;; Session persistence settings
(save-place-mode 1)                 ; Remember cursor position in files
(savehist-mode 1)                   ; Save minibuffer history
(setq savehist-file "~/.emacs.d/savehist")

;; Recent files mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)  ; Keep track of 100 recent files

;; Buffer naming configuration
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-min-dir-content 1)
(setq uniquify-strip-common-suffix t)

;; Window management
(winner-mode 1)                     ; Enable window configuration undo/redo

;; Function name display
(which-function-mode 1)             ; Show current function in header
(setq which-func-unknown "n/a")
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))

;; Tab bar configuration
(tab-bar-mode 1)
(setq tab-bar-show 1)

;; Customize tab bar colors
(set-face-attribute 'tab-bar nil :background "white")
(set-face-attribute 'tab-bar nil :foreground "black")
(set-face-attribute 'tab-bar-tab nil :background "grey90")
(set-face-attribute 'tab-bar-tab nil :foreground "black")
(set-face-attribute 'tab-bar-tab-inactive nil :background "grey80")
(set-face-attribute 'tab-bar-tab-inactive nil :foreground "grey40")
(set-face-attribute 'tab-bar-tab-inactive nil :underline nil)
(set-face-attribute 'tab-bar-tab-inactive nil :box nil)
(set-face-attribute 'tab-bar-tab nil :box nil)
(set-face-attribute 'tab-bar-tab nil :underline nil)

;; Platform-specific font settings
(cond
 ((eq system-type 'darwin)
  (set-frame-font "Comic Code-13")
  (set-face-attribute 'tab-bar nil :font "Comic Code-14"))
 ((eq system-type 'windows-nt)
  (set-frame-font "Comic Code-10")
  (set-face-attribute 'tab-bar nil :font "Comic Code-10")))

;; ============================================================================
;; SECTION 2: PACKAGE MANAGEMENT AND INSTALLATION
;; ============================================================================

;; Set up package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Define packages to install
(setq package-selected-packages '(evil
                                  evil-leader
                                  evil-commentary
                                  evil-numbers
				  evil-surround
				  gruvbox-theme
				  solarized-theme
                                  nyan-mode
                                  flycheck
                                  ripgrep
                                  vertico
                                  rg
                                  copilot-chat
                                  exec-path-from-shell
                                  ))

;; Initialize and refresh package system if needed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Initialize exec-path-from-shell on macOS
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Setup Copilot using use-package
(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
       :rev :newest
       :branch "main")
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  ;; use ctrl space to suggest completion
  (define-key copilot-completion-map (kbd "<C-TAB>") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "<C-S-TAB>") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "<C-return>") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "<C-S-return>") 'copilot-accept-completion-by-line)

  (setq copilot-indent-offset-warning-disable t
        copilot-enable-predicates '(evil-insert-state-p)
	copilot-idle-delay 1
        copilot-max-char -1)
  )

;; ============================================================================
;; SECTION 3: EVIL MODE CONFIGURATION
;; ============================================================================

;; Basic Evil setup
(require 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)

;; Evil surround setup
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Evil leader setup
(require 'evil-leader)
(global-evil-leader-mode)

;; Evil commentary
(require 'evil-commentary)
(evil-commentary-mode)

;; Basic Evil navigation bindings
(evil-define-key 'normal 'global (kbd "] <tab>") 'tab-next)
(evil-define-key 'normal 'global (kbd "[ <tab>") 'tab-previous)
(evil-define-key 'normal 'global (kbd "]q") 'next-error)
(evil-define-key 'normal 'global (kbd "[q") 'previous-error)
(evil-define-key 'insert 'global (kbd "C-S-n") 'copilot-complete)

;; ============================================================================
;; SECTION 4: EDITING AND CODING ENHANCEMENTS
;; ============================================================================

;; Flycheck configuration
(require 'flycheck)
(setq flycheck-checker-error-threshold nil)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-locate-config-file-functions '(flycheck-locate-config-file-ancestor-directories))
(setq flycheck-check-syntax-automatically '(save mode-enabled))

;; Windows specific Flycheck setup
(when (eq system-type 'windows-nt)
  (setq-default flycheck-python-flake8-executable "h:/src/python/python.exe")
  (setq-default flycheck-jshintrc "C:/ZogoTech/hg/cg.dotfiles/jshint.config"))

;; Add Flycheck hooks for various modes
(add-hook 'js-mode-hook #'flycheck-mode)
(add-hook 'python-mode-hook #'flycheck-mode)
(add-hook 'python-ts-mode-hook #'flycheck-mode)

;; Flycheck display customization
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(python-pyright python-pylint python-mypy python-pycompile))
  )

;; Configure Flycheck error list display
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.33)))

;; Flycheck background color change based on errors
(defvar-local my-flycheck-background-cookie nil
  "Cookie for Flycheck background face remapping.")

(defun my-flycheck-change-background ()
  "Change buffer background based on Flycheck error severity."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errors (alist-get 'error counts))
         (warnings (alist-get 'warning counts))
         (bg (cond (errors "misty rose")
                   (warnings "light yellow")
                   (t nil))))
    ;; Remove existing background
    (when my-flycheck-background-cookie
      (face-remap-remove-relative my-flycheck-background-cookie)
      (setq my-flycheck-background-cookie nil))
    ;; Apply new background if needed
    (when bg
      (setq my-flycheck-background-cookie
            (face-remap-add-relative 'default :background bg)))))

(add-hook 'flycheck-after-syntax-check-hook #'my-flycheck-change-background)
(add-hook 'flycheck-mode-hook
          (lambda () (unless flycheck-mode (my-flycheck-change-background))))

;; Navigation in Flycheck errors
(evil-define-key 'normal 'global (kbd "]a") 'flycheck-next-error)
(evil-define-key 'normal 'global (kbd "[a") 'flycheck-previous-error)

;; Vertico completion setup
(require 'vertico)
(setq completion-styles '(basic substring partial-completion flex))
(setq completion-ignore-case t)
(vertico-mode 1)

;; Ediff configuration
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defun my-ediff-restore-windows ()
  "Restore the previous window configuration after an ediff session."
  (winner-undo))
(add-hook 'ediff-quit-hook #'my-ediff-restore-windows)

;; ============================================================================
;; SECTION 5: PYTHON DEVELOPMENT CONFIGURATION
;; ============================================================================

;; Determine which Python mode hook to use (treesit if available)
(setq my-python-hook (if (treesit-available-p) 'python-ts-mode-hook 'python-mode-hook))
;; (setq my-python-hook 'python-mode-hook)

;; Install treesitter Python grammar if not already available
;; Note: This might be better placed elsewhere, as it tries to run every time
;; Emacs starts until it succeeds. Consider manual install or using :ensure with
;; a package definition if python-ts-mode is managed by package.el.
(unless (treesit-language-available-p 'python) ; Use treesit-language-available-p
  (treesit-install-language-grammar 'python))

;; Setup treesitter mode remapping for Python
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

;; Fix for hanging indents
(defun rm-advice-python-indent-line-function ()
  "Add one indentation level when line begins with closing parenthesis."
  ;; remember where we are, and move back to the start of indentation
  (let ((point-current (point)))
    (back-to-indentation)
    (let ((chars-after-indentation (- point-current (point))))
      ;; if line begins with any form of closing parenthesis, add indent level
      (if (looking-at "[])}]")
          (insert (make-string python-indent-offset ?\s)))
      ;; if cursor was to the right of indentation, then move right to
      ;; where it was; otherwise, leave cursor at indentation
      (if (> chars-after-indentation 0)
          (right-char chars-after-indentation)))))

(advice-add 'python-indent-line-function :after
            #'rm-advice-python-indent-line-function)


;; Python-specific settings
(add-hook my-python-hook
          (lambda ()
	    ;; unset the local indent-region-function so it uses the fallback
	    (setq-local indent-region-function nil)
            ;; --- Core Indentation Settings ---
            ;; 1. Use spaces, never hard tabs
            (setq-local indent-tabs-mode nil)

            ;; 2. Set indentation level to 4 spaces
            (setq-local python-indent-offset 4)
            (setq python-indent-offset 4)
            (setq python-continuation-offset 4)

            ;; 3. (Optional but recommended) Ensure tab *display* width is also 4
            ;; This affects how existing tab characters are displayed, if any.
            (setq-local tab-width 4)
            ;; --- End Core Indentation Settings ---

            ;; You might not even need this binding anymore, as python-mode
            ;; usually sets up TAB correctly when the above variables are set.
            ;; Try commenting it out first to see if default behavior works.
            ;; (local-set-key (kbd "TAB") 'indent-relative)

            ;; Set some evil leader bindings
            (evil-leader/set-key

	      "=" 'my-pyyapf-format-block  ;; Format block with yapf
             ";" (lambda ()
                   (interactive)
                   ;; Grab the indentation from the current line
                   (let ((indentation (save-excursion
                                        (back-to-indentation)
                                        (buffer-substring-no-properties (line-beginning-position)
                                                                        (point)))))
                     ;; Move to start of current line
                     (beginning-of-line)
                     ;; Open a blank line above
                     (open-line 1)
                     ;; Insert indentation and `breakpoint();`
                     (insert indentation "breakpoint();")
                     )))))

;; custom indentation function
(defun my-python-select-block ()
  "Select the current block of code in Python mode."
  (interactive)
  (evil-visual-line (python-nav-beginning-of-block)
		    (python-nav-end-of-block))
  (python-nav-end-of-block)
  )

(defun my-pyyapf-format-block (begin end)
  "Format the selected region using pyyapf.py, or if no region is active, the current block of code."
  (interactive "r")
  (unless (use-region-p)
    (save-excursion
      (python-nav-beginning-of-block)
      ;; go to beginning of line
      (beginning-of-line)
      (setq begin (point))
      (python-nav-end-of-block)
      (setq end (point))))
  (shell-command-on-region begin end "pyyapf.py" nil t))

;; Python-specific Z functions
(defun z-sort-imports()
  "Sorts and spaces python imports in a case insensitive zt way"
  (interactive)
  (let ((sort-fold-case t))
    (mark-paragraph)
    (align-regexp (region-beginning) (region-end) "from\\b.*?\\(\\s-*\\)import")
    (sort-lines nil (region-beginning) (region-end))
    (search-forward "import")
    )
  )

(defun z-get-import()
  "Insert an import that is already used in your project"
  (interactive)
  ;; Move cursor to the beginning of the line
  (beginning-of-line)
  (let (
        (file-lines
         (split-string
          (shell-command-to-string (concat "cd " my-project-root
                                   " && "
                                   "rg --follow "
                                   "-I \"^from\\s+\\S+\\s+import\\s+\\S+.*$\" "
                                   " | tr -s \" \" "
                                   " | uniq "
                                   " | sort"
                                   ))
          "\n"))
        (tbl (make-hash-table :test 'equal))
        (ido-list))
    (mapc (lambda (line)
            "Put each line in a hash table as well as ido-list."
            (puthash line line tbl)
            (push line ido-list)
            )
          file-lines)
    (let
        (
         (import (gethash (completing-read "? " ido-list) tbl)))
      (insert import)
      (insert "\n")
      (z-sort-imports)
      )
    ))

;; ============================================================================
;; SECTION 6: PROJECT MANAGEMENT
;; ============================================================================

;; Project variables
(setq my-project-root (expand-file-name "."))
(setq my-project-prist my-project-root)
(setq my-ctags-languages "")
(setq my-ctags-kinds "")
(setq my-ctags-excludes "")
(setq my-complete-find-file-command "rg --files --path-separator / ")
(setq my-diff-command "diff -qr -x .hg -x .git -x etags")

;; Project initialization
(defun my-project-init()
  (interactive)
  (setq my-project-root (expand-file-name (directory-file-name (read-directory-name "Project root: " my-project-root))))
  (setq my-project-prist my-project-root)
  (setq my-complete-find-file-command "rg --files --path-separator / -tpy -tjs -tcss -ttxt ")
  (setq my-diff-command "diff -qr -x .hg -x .git -x etags")
  ;; Set the window title to the project root
  (setq frame-title-format my-project-root)
  (cd my-project-root)
  (find-file my-project-root)
  (message "project initialized")
  )

;; Project-specific ripgrep search
(defun my-project-rg ()
  (interactive)
  (let* ((default-query (thing-at-point 'word t))
         (query (read-string (format "rg (default: %s): " default-query) nil nil default-query))
         (file-ext (format "*. %s" (file-name-extension (or (buffer-file-name) ""))))
         (files (read-string (format "Files to search (default: %s): " file-ext) nil nil file-ext))
         (dir (directory-file-name (read-directory-name "Directory: " my-project-root))))
    (rg query files dir)))

;; Project-wide replace under cursor
(defun my-project-replace-under-cursor()
  (interactive)
  (if (use-region-p)
      (let (
            (selection
             (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
            (message "empty string")
          (evil-ex (concat ".,$s/" selection "/"))
          ))
    (evil-ex (concat ".,$s/" (thing-at-point 'word) "/"))))

;; Project window splitting
(defun my-project-split-right ()
  "Split the current window once to the right, then force all but the
   right-most windows in the frame to be 85 columns wide."
  (interactive)
  (evil-window-vsplit)
  (let* ((target-width 85)
         ;; Get all windows in the *selected frame*, skipping minibuffers.
         ;; Then filter out non-window objects (just in case).
         (ws (sort (cl-remove-if-not #'windowp
                                     (window-list (selected-frame) 'no-minibuffer))
                   (lambda (w1 w2)
                     (< (window-left-column w1)
                        (window-left-column w2))))))
    ;; Resize each window except the last one
    (dolist (w (butlast ws))
      (with-selected-window w
        (let ((delta (- target-width (window-total-width w))))
          (ignore-errors
            (window-resize w delta t)))))))

;; Code review functionality
(define-minor-mode my-code-review-mode
  "Minor mode for a code review"
  :init-value nil
  :lighter "my-code-review"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<return>") 'my-open-diff)
            map)
  )

(defun my-open-diff()
  (interactive)
  (let (
        (parts (split-string (thing-at-point 'line) " "))
        )
    (ediff (nth 1 parts) (nth 3 parts))
    ))

(defun my-code-review ()
  "Compare source to pristine copy."
  (interactive) ;; Allows the function to be called interactively
  ;; If a buffer named "code-review" exists, kill it
  (when-let ((existing-buf (get-buffer "code-review")))
    (kill-buffer existing-buf))
  ;; Create a new buffer named "code-review"
  (let (($buf (generate-new-buffer "code-review")))
    ;; Create a new tab and switch to it
    (tab-new)
    (switch-to-buffer $buf)
    ;; Set up the buffer with the necessary modes
    (text-mode)             ;; Simple text editing mode
    (my-code-review-mode)   ;; Custom mode for code review
    (hl-line-mode)          ;; Highlight the current line
    ;; Prompt the user for the left directory and use it as the default for the right directory
    (let* ((left-dir (expand-file-name (directory-file-name
                                        (read-directory-name "left: " my-project-prist))))
           (right-dir (expand-file-name (directory-file-name
                                         (read-directory-name "right: " my-project-root))))
           (cmd (concat
                 "cd / && "
                 my-diff-command
                 (shell-quote-argument left-dir)
                 " "
                 (shell-quote-argument right-dir))))
      ;; Print the constructed command string to the Emacs messages buffer
      (message "cmd: %s" cmd)
      ;; Run the shell command and insert its output into the buffer
      (insert (shell-command-to-string cmd)))
    ;; Return the buffer object representing the new buffer
    $buf))

;; ============================================================================
;; SECTION 7: TAGS AND NAVIGATION
;; ============================================================================

;; Tags configuration
(setq tags-case-fold-search nil)

;; Generate and update tags
(defun get-my-ctags-command()
  (concat "ctags -Ref " my-project-root "/etags " my-project-root))

(defun my-update-tags ()
  (interactive)
  (message (get-my-ctags-command))
  (shell-command (get-my-ctags-command))
  (visit-tags-table (concat my-project-root "/etags"))
  )

;; Improved Xref definitions finder with completion
(defun my-xref-find-definitions-with-completion (identifier)
  "Find definitions of IDENTIFIER using completing-read for selection."
  (interactive (list (xref--read-identifier "Find definitions of: ")))
  (let* ((fetcher (xref--fetcher-for-identifier identifier))
         (xrefs (funcall fetcher))
         (xrefs-alist nil))
    (if (not xrefs)
        (message "No definitions found for: %s" identifier)
      (xref--with-collection xrefs
        (let ((candidates nil))
          ;; Build list of candidates with nice display format
          (xref--collection-table
           (lambda (xref)
             (let* ((loc (xref-location xref))
                    (file (or (xref-location-group loc) ""))
                    (line (xref-location-line loc))
                    (column (xref-location-column loc))
                    (summary (xref-match-summary xref))
                    (display (format "%s:%s:%s: %s"
                                     (file-name-nondirectory file)
                                     (or line "")
                                     (or column "")
                                     (or summary ""))))
               (push (cons display xref) candidates))))
          (if (= (length candidates) 1)
              ;; If only one candidate, just go there
              (xref-pop-to-location (cdar candidates) nil)
            ;; Otherwise use completing-read
            (let* ((selection (completing-read "Choose definition: "
                                              (mapcar #'car candidates) nil t))
                   (selected-xref (cdr (assoc selection candidates))))
              (xref-pop-to-location selected-xref nil))))))))

;; ============================================================================
;; SECTION 8: COMPLETION FUNCTIONS
;; ============================================================================

;; Generic completer functions
(defun my-completer (list-of-strings sink-fn)
  "A generic completer that takes a command and a sink function."
  (interactive)
  (let (
        (tbl (make-hash-table :test 'equal)))
    (mapc (lambda (line) (puthash line line tbl)) list-of-strings)
    (funcall sink-fn (gethash
                      (completing-read "? " (hash-table-keys tbl)) tbl))))

(defun my-completer-cmd (cmd sink-fn &optional cache-file force-refresh)
  "A generic completer that takes a command and a sink function, with optional caching."
  (interactive)
  (let* ((cache-dir (expand-file-name "~/.cache/emacs-completers"))
        (cache-path (when cache-file (expand-file-name cache-file cache-dir))))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))
    (if (and cache-path (file-exists-p cache-path) (not force-refresh))
        (with-temp-buffer
          (insert-file-contents cache-path)
          (my-completer (split-string (buffer-string) "\n" t) sink-fn))
      (let ((result (split-string (shell-command-to-string cmd) "\n" t)))
        (when cache-path
          (with-temp-file cache-path
            (insert (string-join result "\n"))))
        (my-completer result sink-fn)))))

;; Various completion commands
(defun my-complete-find-init (&optional force-refresh)
  (interactive "P")
  (my-completer-cmd (concat "rg --files " (expand-file-name "~/.emacs.d")) 'find-file "find-init.cache" force-refresh))

(defun my-complete-find-file (&optional force-refresh)
  (interactive "P")
  (my-completer-cmd (concat my-complete-find-file-command "\"" my-project-root "\"") 'find-file "find-file.cache" force-refresh))

(defun my-complete-find-file-force ()
  (interactive)
  (my-complete-find-file t))

(defun my-complete-buffer-tags (&optional force-refresh)
  "Complete buffer tags."
  (interactive "P")
  (my-completer-cmd
   (concat "ctags -n --python-kinds=cfmv -f- " buffer-file-name)
   (lambda (line)
     (let ((spl (split-string line "\t")))
       (goto-line (string-to-number (substring (nth 2 spl) 0 -2)))))
   "buffer-tags.cache" force-refresh))

(defun my-complete-buffer-tags-force ()
  (interactive)
  (my-complete-buffer-tags t))

(defun my-complete-recentf ()
  "Complete using recentf and open it."
  (interactive)
  (my-completer recentf-list (lambda (line) (find-file line))))

(defun my-complete-buffers()
  "Select a buffer to switch to using completing-read."
  (interactive)
  (my-completer
   (mapcar 'buffer-name (buffer-list))
   (lambda (line) (switch-to-buffer line))))

(defun my-complete-kill-ring ()
  "Select from kill ring and insert at point."
  (interactive)
  (unless kill-ring
    (error "Kill ring is empty"))
  ;; Temporarily override completion styles to 'basic to avoid sorting
  (let ((vertico-sort-function 'nil))
    (setq selection (completing-read "Select from kill ring: " kill-ring nil t))
    (when selection
      (insert selection))))

(defun my-complete-project-tags (&optional force-refresh)
  "Complete project tags."
  (interactive "P")
  (my-completer-cmd
   (string-join
    (cl-remove-if #'string-empty-p
                  (list "ctags --recurse -xf -"
                        my-ctags-excludes
                        my-ctags-languages
                        my-ctags-kinds
                        my-project-root))
    " ")
   (lambda (line)
     (message line)
     (let ((spl (split-string line)))
       (message "%s" (prin1-to-string spl))
       (let ((filename (nth 3 spl))
             (line (string-to-number (nth 2 spl)))
             )
         (find-file filename)
         (goto-line line)
         )))
   "project-tags.cache" force-refresh))

(defun my-complete-project-tags-force()
  (interactive)
    (my-complete-project-tags t))

;; ============================================================================
;; SECTION 9: PLATFORM-SPECIFIC SETTINGS
;; ============================================================================

;; Windows-specific functions
(when (eq system-type 'windows-nt)
  ;; Get junction target function
  (defun get-junction-target (junction-dir)
    "Return the target directory of a Windows junction at JUNCTION-DIR.
    This function uses the Windows command `fsutil reparsepoint query` to
    retrieve the reparse point data. If the directory is a valid junction,
    the returned target will have the leading \"\\??\\\" prefix removed.
    If not, an error is signaled."
    (unless (eq system-type 'windows-nt)
      (error "This function works only on Windows systems"))
    (unless (file-directory-p junction-dir)
      (error "Not a valid directory: %s" junction-dir))
    (let* ((cmd (format "fsutil reparsepoint query %s" (shell-quote-argument junction-dir)))
           (output (shell-command-to-string cmd)))
      (if (string-match "Substitute Name:\\s-*\\(.*\\)" output)
          (let ((target (string-trim (match-string 1 output))))
            ;; Remove the Windows internal prefix if present.
            (if (string-prefix-p "\\??\\" target)
                (substring target 4)
              target))
        (error "Directory %s is not a junction or its target could not be determined" junction-dir))))

  ;; Windows specific project initialization
  (defun z-init()
    (interactive)
    (setq my-project-root (get-junction-target "h:/dev"))
    (setq my-tags (concat my-project-root "/etags"))
    (setq my-project-prist (get-junction-target "h:/prist"))
    (setq my-complete-find-file-command "rg --files --path-separator / -tpy -tjs -tcss -ttxt ")
    (setq my-diff-command "diff -qr -x .hg -x *.pyc -x __pycache__ -x zebra -x etags -x nvim-undo -x vim-undo -x tags ")
    (setq frame-title-format (string-trim (shell-command-to-string "z_get_proj")))
    ;; if etags doesn't exist, then make it
    (unless (file-exists-p my-tags) (my-update-tags))
    (visit-tags-table my-tags)
    (message "the z has been initted for windows...")
    )

  ;; Windows regression function
  (defun z-open-regression ()
    "Open the regression folder, refresh its contents and sort files by time."
    (interactive)
    ;; Create a new tab
    (tab-new)
    ;; Open the regression directory in Dired mode in the new tab
    (dired "\\\\regr3.ztaustin.local\\home\\cg\\regression-output")
    ;; Refresh Dired buffer
    (revert-buffer)
    ;; Return should open file in a new window not replace the buffer
    (define-key dired-mode-map (kbd "RET") 'dired-find-file-other-window)
    ))

;; macOS specific initialization
(when (eq system-type 'darwin)
  (defun z-init()
    (interactive)
    (setq my-project-root (expand-file-name "~/z/dev"))
    (defun my-complete-find-file (&optional force-refresh)
      (interactive "P")
      (my-completer-cmd (concat "fd . "
                                " --extension py "
                                " --extension js "
                                " --extension css "
                                my-project-root "/(server|webserver|appserver)"
                                ) 'find-file "find-file.cache" force-refresh))
    (message "the z has been initted for macos...")
    ))

;; ============================================================================
;; SECTION 10: NYAN MODE AND FUN STUFF
;; ============================================================================

;; Nyan cat progress bar (only in GUI mode)
(require 'nyan-mode)
(if (display-graphic-p) (progn (nyan-mode 1)))

;; ============================================================================
;; SECTION 11: DEBUGGER CONFIGURATION
;; ============================================================================

;; Setup for debugger usage
(defun my-debugger-setup()
  (interactive)
  ;; Prevent Emacs from raising its frame when the server opens a file
  (global-flycheck-mode 0)
  ;; Turn on line numbers no matter what
  (display-line-numbers-mode 1)
  (setq server-raise-frame nil)
  (server-start)
  (global-hl-line-mode 1)
  (load-theme 'solarized-gruvbox-light t)
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (global-display-line-numbers-mode 1)
  )

;; ============================================================================
;; SECTION 12: CUSTOM KEYBINDINGS
;; ============================================================================

;; Set keybindings with evil-leader
(evil-leader/set-key
  ;; File navigation
  "i" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))  ;; Open init file
  "o" (lambda () (interactive) (find-file "~/.emacs.d/todo.org")) ;; Open todo.org
  "e" 'my-complete-find-file-force                                ;; Find files
  "E" 'my-complete-buffers
  "b" 'my-complete-buffer-tags-force                              ;; Complete buffer tags
  "<tab>" 'tab-close                                              ;; Close current tab

  ;; Project management
  "pp" 'my-project-init                                           ;; Initialize project
  "v" 'my-project-split-right                                     ;; Split right
  "r" 'my-project-replace-under-cursor                            ;; Replace under cursor

  ;; Tags and search
  "t" (lambda () (interactive) (evil-jump-to-tag t))              ;; Jump to tag
  "T" 'find-tag                                                   ;; Find tag
  "xt" 'my-update-tags                                            ;; Update tags
  "g" (lambda () (interactive)                                    ;; Interactive grep search
        (let* ((default-query (or (thing-at-point 'symbol t) ""))
               (default-files (if (buffer-file-name)
                                  (let ((ext (file-name-extension (buffer-file-name))))
                                    (if ext (concat "*." ext) "*"))
                                "*.py"))
               (default-dir my-project-root)
               (query (read-string (format "RG Query (default \"%s\"): " default-query)
                                   nil nil default-query))
               (files (read-string (format "Files (default \"%s\"): " default-files)
                                   nil nil default-files))
               (dir (read-directory-name (format "Directory (default \"%s\"): " default-dir)
                                         default-dir default-dir t)))
          (rg query files dir)))

  ;; Editing and buffer management
  "xb" 'eval-buffer                                               ;; Evaluate buffer
  "n" 'display-line-numbers-mode                                  ;; Toggle line numbers
  "C-a" 'evil-numbers/inc-at-pt                                   ;; Increment number
  "C-x" 'evil-numbers/dec-at-pt                                   ;; Decrement number
  "q" 'evil-quit                                                  ;; Quit

  ;; Version control and diff
  "H" (lambda () (interactive)
        (if (vc-registered (buffer-file-name))
            (progn
              (tab-new)
              (call-interactively 'vc-ediff))
          (message "File is not under version control")))

  ;; Z-specific functions
  "zz" 'z-init                                                    ;; Initialize Z project
  "zc" 'my-code-review                                            ;; Code review mode
  "zr" 'z-open-regression                                         ;; Open regression test folder
  "si" 'z-sort-imports                                            ;; Sort Python imports
  "zi" 'z-get-import                                              ;; Get existing Python import
  "c" 'copilot-mode
  )

;; Set up keybindings for rg-mode
(require 'rg)
(define-key rg-mode-map (kbd "]q") 'next-error)
(define-key rg-mode-map (kbd "[q") 'previous-error)

;; misc keybindings
;; shift tab should insert an actual tab
(define-key evil-insert-state-map (kbd "<S-tab>") (lambda () (interactive) (insert "\t")))

;; ============================================================================
;; SECTION 13: MISCELLANEOUS AND CUSTOMIZATION
;; ============================================================================

;; Special utility function for cleaning up the kill buffer
(defun z-find-kill (beginning end)
  "Take what is in the clipboard and remove matches from file"
  (interactive "r")
  (defalias 'rpex 'replace-regexp-in-string)
  (let ( ;; set local vars
        (my-query
         (rpex (read-string "SITE (Enter for \"ALVIN\"): " nil nil "ALVIN") ".*"  ;; site
               (rpex "\\s[0-9]+:[0-9]+:[0-9]\\s" ".*"
                     (rpex "[0-9]+" ".*"  ;; numbers
                           (regexp-quote (buffer-substring beginning end)))))))
    (query-replace-regexp my-query "")
    ))

;; Load shell customizations
(load "~/.emacs.d/myshell.el")

;; Custom settings - automatically added by Emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(copilot-chat-model "claude-3.7-sonnet")
 '(custom-safe-themes
   '("57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7"
     "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9"
     "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176" default))
 '(package-selected-packages '(copilot))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main")))
 '(python-indent-block-paren-deeper t)
 '(warning-suppress-log-types
   '((copilot copilot-exceeds-max-char) (copilot copilot-exceeds-max-char)))
 '(xref-show-definitions-function 'xref-show-definitions-completing-read))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
