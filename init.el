;;; init.el --- Clean Emacs configuration
;;; Commentary:
;;; A streamlined Emacs setup with evil-mode and development tools.
;;; Code:

;; Basic settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen 1)

(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(display-fill-column-indicator-mode)
(setq my-line-indicator-color "misty rose")
(set-face-background 'fill-column-indicator my-line-indicator-color)
(set-face-foreground 'fill-column-indicator my-line-indicator-color)

(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq mode-require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq echo-keystrokes .1)
(setq ring-bell-function 'ignore)

;; Backup and autosave
(unless (file-directory-p "~/.emacs.d/autosaves")
  (make-directory "~/.emacs.d/autosaves"))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups/")))
(setq create-lockfiles nil)

;; Session persistence
(save-place-mode 1)
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist")

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-min-dir-content 1)
(setq uniquify-strip-common-suffix t)

(winner-mode 1)

;; Function display
(which-function-mode 1)
(setq which-func-unknown "n/a")
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))

;; Tab bar
(tab-bar-mode 1)
(setq tab-bar-show 1)

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

;; Fonts
(cond ((eq system-type 'darwin)
       (set-frame-font "Comic Code-13")
       (set-face-attribute 'tab-bar nil :font "Comic Code-14"))
      ((eq system-type 'windows-nt)
       (set-frame-font "Comic Code-10")
       (set-face-attribute 'tab-bar nil :font "Comic Code-10")))

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Evil mode - Load first for immediate availability
(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global (kbd "] <tab>") 'tab-next)
  (evil-define-key 'normal 'global (kbd "[ <tab>") 'tab-previous)
  (evil-define-key 'normal 'global (kbd "]q") 'next-error)
  (evil-define-key 'normal 'global (kbd "[q") 'previous-error)
  (evil-define-key 'insert 'global (kbd "C-S-n") 'copilot-complete))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-numbers
  :ensure t
  :after evil)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Polymode setup
(use-package polymode
  :ensure t
  :config
  (define-hostmode poly-python-hostmode
    :mode 'python-mode)

  (define-innermode poly-python-sql-innermode
    :mode 'sql-mode
    :head-matcher "\\(?:[fF]\\)?\\(?:\"\"\"\\|'''\\)[ \t]*--sql[^\n]*\n"
    :tail-matcher "[ \t]*\\(?:\"\"\"\\|'''\\)"
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host)

  (define-innermode poly-css-innermode
    :mode 'css-mode
    :head-matcher "style='''"
    :tail-matcher "'''")

  (define-polymode poly-python-z-mode
    :hostmode 'poly-python-hostmode
    :innermodes '(poly-python-sql-innermode poly-css-innermode))

  (defun my-polymode-inner-setup ()
    (when (or (eq major-mode 'css-mode)
              (eq major-mode 'sql-mode))
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 2))))

(defun my-toggle-python-sql-mode ()
  "Toggle polymode for Python SQL strings."
  (interactive)
  (if (eq major-mode 'poly-python-sql-mode)
      (python-mode)
    (poly-python-sql-mode)))

;; Org-mode fix for evil
;; Fix tab behavior in org-mode with evil-mode
(with-eval-after-load 'evil
  (with-eval-after-load 'org
    ;; Allow tab to work for folding/unfolding in org-mode
    (evil-define-key '(normal) org-mode-map (kbd "<tab>") #'org-cycle)
    (evil-define-key '(normal) org-mode-map (kbd "TAB") #'org-cycle)

    ;; For visibility cycling on a global level (shift+tab)
    (evil-define-key '(normal) org-mode-map (kbd "<S-tab>") #'org-global-cycle)
    (evil-define-key '(normal) org-mode-map (kbd "<backtab>") #'org-global-cycle)

    ;; For insert mode tab behavior
    (evil-define-key '(insert) org-mode-map (kbd "<tab>") #'org-cycle)
    (evil-define-key '(insert) org-mode-map (kbd "TAB") #'org-cycle)))


;; time search
(defun my-time-search ()
  "Set up outline-minor-mode to fold on timestamp lines.
   Enables folding for lines starting with YYYY-MM-DD format."
  (interactive)
  (outline-minor-mode 1)
  (setq-local outline-regexp "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (message "Time-based folding enabled. Use C-c @ C-q to hide all, C-c @ C-a to show all, C-c @ C-t to toggle."))


(use-package copilot
  :disabled t
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
       :rev :newest
       :branch "main")
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "<C-TAB>") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "<C-S-TAB>") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "<C-return>") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "<C-S-return>") 'copilot-accept-completion-by-line)
  (setq copilot-indent-offset-warning-disable t
        copilot-enable-predicates '(evil-insert-state-p)
        copilot-idle-delay 0.2
        copilot-max-char -1))

(use-package copilot-chat
  :ensure t)

;; Flycheck
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
    (when my-flycheck-background-cookie
      (face-remap-remove-relative my-flycheck-background-cookie)
      (setq my-flycheck-background-cookie nil))
    (when bg
      (setq my-flycheck-background-cookie
            (face-remap-add-relative 'default :background bg)))))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-checker-error-threshold nil)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-locate-config-file-functions '(flycheck-locate-config-file-ancestor-directories))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (when (eq system-type 'windows-nt)
    (setq-default flycheck-python-flake8-executable "h:/src/python/python.exe")
    (setq-default flycheck-jshintrc "C:/ZogoTech/hg/cg.dotfiles/jshint.config"))
  :config
  (setq-default flycheck-disabled-checkers '(python-pyright python-pylint python-mypy python-pycompile))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))
  (add-hook 'flycheck-after-syntax-check-hook #'my-flycheck-change-background)
  (add-hook 'flycheck-mode-hook
            (lambda () (unless flycheck-mode (my-flycheck-change-background))))
  (evil-define-key 'normal 'global (kbd "]a") 'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "[a") 'flycheck-previous-error)
  :hook
  ((js-mode . flycheck-mode)
   (python-mode . flycheck-mode)))

;; Vertico completion
(use-package vertico
  :ensure t
  :init
  (setq completion-styles '(basic substring partial-completion flex))
  (setq completion-ignore-case t)
  :config
  (vertico-mode 1))

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun my-ediff-restore-windows ()
  "Restore the previous window configuration after an ediff session."
  (winner-undo))

(add-hook 'ediff-quit-hook #'my-ediff-restore-windows)

;; Python configuration
(setq my-python-hook 'python-mode-hook)

(defun rm-advice-python-indent-line-function ()
  "Add one indentation level when line begins with closing parenthesis."
  (let ((point-current (point)))
    (back-to-indentation)
    (let ((chars-after-indentation (- point-current (point))))
      (if (looking-at "[])}]")
          (insert (make-string python-indent-offset ?\s)))
      (if (> chars-after-indentation 0)
          (right-char chars-after-indentation)))))

(advice-add 'python-indent-line-function :after
            #'rm-advice-python-indent-line-function)

(add-hook my-python-hook
          (lambda ()
            (setq-local indent-region-function nil)
            (setq-local indent-tabs-mode nil)
            (setq-local python-indent-offset 4)
            (setq python-indent-offset 4)
            (setq python-continuation-offset 4)
            (setq-local tab-width 4)

            (evil-leader/set-key
              "=" 'my-pyyapf-format-block
              ";" (lambda ()
                    (interactive)
                    (let ((indentation (save-excursion
                                         (back-to-indentation)
                                         (buffer-substring-no-properties (line-beginning-position)
                                                                         (point)))))
                      (beginning-of-line)
                      (open-line 1)
                      (insert indentation "breakpoint();"))))))

;; Python functions
(defun my-python-select-block ()
  "Select the current block of code in Python mode."
  (interactive)
  (evil-visual-line (python-nav-beginning-of-block)
                    (python-nav-end-of-block))
  (python-nav-end-of-block))

(defun my-pyyapf-format-block (begin end)
  "Format the selected region using pyyapf.py, or if no region is active, the current block of code."
  (interactive "r")
  (unless (use-region-p)
    (save-excursion
      (python-nav-beginning-of-block)
      (beginning-of-line)
      (setq begin (point))
      (python-nav-end-of-block)
      (setq end (point))))
  (shell-command-on-region begin end "pyyapf.py" nil t))

(defun z-sort-imports()
  "Sorts and spaces python imports in a case insensitive zt way"
  (interactive)
  (let ((sort-fold-case t))
    (mark-paragraph)
    (align-regexp (region-beginning) (region-end) "from\\b.*?\\(\\s-*\\)import")
    (sort-lines nil (region-beginning) (region-end))
    (search-forward "import")))

(defun z-get-import()
  "Insert an import that is already used in your project"
  (interactive)
  (beginning-of-line)
  (let ((file-lines
         (split-string
          (shell-command-to-string (concat "cd " my-project-root
                                   " && "
                                   "rg --follow "
                                   "-I \"^from\\s+\\S+\\s+import\\s+\\S+.*$\" "
                                   " | tr -s \" \" "
                                   " | uniq "
                                   " | sort"))
          "\n"))
        (tbl (make-hash-table :test 'equal))
        (ido-list))
    (mapc (lambda (line)
            (puthash line line tbl)
            (push line ido-list))
          file-lines)
    (let ((import (gethash (completing-read "? " ido-list) tbl)))
      (insert import)
      (insert "\n")
      (z-sort-imports))))

;; Project management
(setq my-project-root (expand-file-name "."))
(setq my-project-prist my-project-root)
(setq my-ctags-languages "")
(setq my-ctags-kinds "")
(setq my-ctags-excludes "")
(setq my-complete-find-file-command "rg --files --path-separator / ")
(setq my-diff-command "diff -qr -x .hg -x .git -x etags")

(defun my-project-init()
  (interactive)
  (setq my-project-root (expand-file-name (directory-file-name (read-directory-name "Project root: " my-project-root))))
  (setq my-project-prist my-project-root)
  (setq my-complete-find-file-command "rg --files --path-separator / -tpy -tjs -tcss -ttxt ")
  (setq my-diff-command "diff -qr -x .hg -x .git -x etags")
  (setq frame-title-format my-project-root)
  (cd my-project-root)
  (find-file my-project-root)
  (message "project initialized"))

(defun my-project-rg ()
  (interactive)
  (let* ((default-query (thing-at-point 'word t))
         (query (read-string (format "rg (default: %s): " default-query) nil nil default-query))
         (file-ext (format "*. %s" (file-name-extension (or (buffer-file-name) ""))))
         (files (read-string (format "Files to search (default: %s): " file-ext) nil nil file-ext))
         (dir (directory-file-name (read-directory-name "Directory: " my-project-root))))
    (rg query files dir)))

(defun my-project-replace-under-cursor()
  (interactive)
  (if (use-region-p)
      (let ((selection
             (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
            (message "empty string")
          (evil-ex (concat ".,$s/" selection "/"))))
    (evil-ex (concat ".,$s/" (thing-at-point 'word) "/"))))

(defun my-project-split-right ()
  "Split the current window once to the right, then force all but the
   right-most windows in the frame to be 85 columns wide."
  (interactive)
  (evil-window-vsplit)
  (let* ((target-width 85)
         (ws (sort (cl-remove-if-not #'windowp
                                     (window-list (selected-frame) 'no-minibuffer))
                   (lambda (w1 w2)
                     (< (window-left-column w1)
                        (window-left-column w2))))))
    (dolist (w (butlast ws))
      (with-selected-window w
        (let ((delta (- target-width (window-total-width w))))
          (ignore-errors
            (window-resize w delta t)))))))

;; Code review
(define-minor-mode my-code-review-mode
  "Minor mode for a code review"
  :init-value nil
  :lighter "my-code-review"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<return>") 'my-open-diff)
            map))

(defun my-open-diff()
  (interactive)
  (let ((parts (split-string (thing-at-point 'line) " ")))
    (ediff (nth 1 parts) (nth 3 parts))))

(defun my-code-review ()
  "Compare source to pristine copy."
  (interactive)
  (when-let ((existing-buf (get-buffer "code-review")))
    (kill-buffer existing-buf))
  (let (($buf (generate-new-buffer "code-review")))
    (tab-new)
    (switch-to-buffer $buf)
    (text-mode)
    (my-code-review-mode)
    (hl-line-mode)
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
      (message "cmd: %s" cmd)
      (insert (shell-command-to-string cmd)))
    $buf))

;; Tags
(setq tags-case-fold-search nil)

(defun get-my-ctags-command()
  (concat "ctags -Ref " my-project-root "/etags " my-project-root))

(defun my-update-tags ()
  (interactive)
  (message (get-my-ctags-command))
  (shell-command (get-my-ctags-command))
  (visit-tags-table (concat my-project-root "/etags")))

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
              (xref-pop-to-location (cdar candidates) nil)
            (let* ((selection (completing-read "Choose definition: "
                                              (mapcar #'car candidates) nil t))
                   (selected-xref (cdr (assoc selection candidates))))
              (xref-pop-to-location selected-xref nil))))))))

;; Completion functions
(defun my-completer (list-of-strings sink-fn)
  "A generic completer that takes a command and a sink function."
  (interactive)
  (let ((tbl (make-hash-table :test 'equal)))
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

(defun my-complete-kill-ring-old ()
  "Select from kill ring and insert at point."
  (interactive)
  (unless kill-ring
    (error "Kill ring is empty"))
  (let ((vertico-sort-function 'nil))
    (setq selection (completing-read "Select from kill ring: " kill-ring nil t))
    (when selection
      (insert selection))))

(defun debug-last-command ()
  "Show what the last command was."
  (interactive)
  (message "last-command: %s" last-command))

;; Modified kill ring function with debugging
(defun my-complete-kill-ring ()
  "Select from kill ring and insert at point. If last command was a paste, replace that text."
  (interactive)
  (unless kill-ring
    (error "Kill ring is empty"))
  ;; Debug: show what last-command is
  (message "Debug: last-command is %s" last-command)
  (let ((vertico-sort-function 'nil)
        ;; Check for common Evil paste commands
        (replacing-paste (or (eq last-command 'yank)
                            (eq last-command 'evil-paste-after)
                            (eq last-command 'evil-paste-before)
                            (eq last-command 'evil-put)
                            ;; Add the actual command name once you find it
                            )))
    (setq selection (completing-read "Select from kill ring: " kill-ring nil t))
    (when selection
      (if replacing-paste
          (progn
            (message "Replacing previous paste")
            (delete-region (mark t) (point))
            (insert selection)
            (setq this-command 'yank))
        (progn
          (message "Regular insert (last-command was: %s)" last-command)
          (insert selection))))))

(defun my-complete-kill-ring ()
  "Select from kill ring. Replace selection if active, otherwise insert at point."
  (interactive)
  (unless kill-ring
    (error "Kill ring is empty"))
  (let ((vertico-sort-function 'nil))
    (setq selection (completing-read "Select from kill ring: " kill-ring nil t))
    (when selection
      (if (use-region-p)
          ;; Replace the selected region
          (progn
            (delete-region (region-beginning) (region-end))
            (insert selection))
        ;; Just insert at point
        (insert selection)))))

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
             (line (string-to-number (nth 2 spl))))
         (find-file filename)
         (goto-line line))))
   "project-tags.cache" force-refresh))

(defun my-complete-project-tags-force()
  (interactive)
  (my-complete-project-tags t))

(defun my-complete-shell-history ()
  "Complete from current shell buffer's command history."
  (interactive)
  (unless (derived-mode-p 'comint-mode)
    (error "Not in a comint-mode buffer"))
  (unless comint-input-ring
    (error "No command history available"))
  (let ((history-list (ring-elements comint-input-ring)))
    (my-completer history-list
                  (lambda (line)
                    (comint-send-string (get-buffer-process (current-buffer))
                                        (concat line "\n"))))))

;; Platform-specific settings
(when (eq system-type 'windows-nt)
  (defun get-junction-target (junction-dir)
    "Return the target directory of a Windows junction at JUNCTION-DIR."
    (unless (eq system-type 'windows-nt)
      (error "This function works only on Windows systems"))
    (unless (file-directory-p junction-dir)
      (error "Not a valid directory: %s" junction-dir))
    (let* ((cmd (format "fsutil reparsepoint query %s" (shell-quote-argument junction-dir)))
           (output (shell-command-to-string cmd)))
      (if (string-match "Substitute Name:\\s-*\\(.*\\)" output)
          (let ((target (string-trim (match-string 1 output))))
            (if (string-prefix-p "\\??\\" target)
                (substring target 4)
              target))
        (error "Directory %s is not a junction or its target could not be determined" junction-dir))))

  (defun z-init()
    (interactive)
    (setq my-project-root (get-junction-target "h:/dev"))
    (setq my-tags (concat my-project-root "/etags"))
    (setq my-project-prist (get-junction-target "h:/prist"))
    (setq my-complete-find-file-command "rg --files --path-separator / -tpy -tjs -tcss -ttxt ")
    (setq my-diff-command "diff -qr -x .hg -x *.pyc -x __pycache__ -x zebra -x etags -x nvim-undo -x vim-undo -x tags ")
    (setq frame-title-format (string-trim (shell-command-to-string "z_get_proj")))
    (unless (file-exists-p my-tags) (my-update-tags))
    (visit-tags-table my-tags)
    (message "the z has been initted for windows..."))

  (defun z-open-regression ()
    "Open the regression folder, refresh its contents and sort files by time."
    (interactive)
    (tab-new)
    (dired "\\\\regr3.ztaustin.local\\home\\cg\\regression-output")
    (revert-buffer)
    (define-key dired-mode-map (kbd "RET") 'dired-find-file-other-window)))

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
                                my-project-root "/(server|webserver|appserver)")
                        'find-file "find-file.cache" force-refresh))
    (message "the z has been initted for macos...")))

;; Nyan mode
(use-package nyan-mode
  :ensure t
  :if (display-graphic-p)
  :config
  (nyan-mode 1))

;; Debugger setup
(defun my-debugger-setup()
  (interactive)
  (global-flycheck-mode 0)
  (display-line-numbers-mode 1)
  (setq server-raise-frame nil)
  (server-start)
  (global-hl-line-mode 1)
  (load-theme 'solarized-gruvbox-light t)
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (global-display-line-numbers-mode 1))

;; Custom selective display increment/decrement functions
;; These functions manage the selective-display variable with increment/decrement
;; When it reaches 0, it's set to nil (same as C-x $)

(defun my-sd-inc ()
  "Increment selective display level by 1.
If selective-display is nil, set it to 1.
If it's already a number, increment by 1."
  (interactive)
  (setq selective-display
        (cond
         ((null selective-display) 1)
         ((numberp selective-display) (1+ selective-display))
         (t 1)))
  (message "Selective display level: %s" selective-display)
  ;; Force redisplay
  (redraw-display))

(defun my-sd-dec ()
  "Decrement selective display level by 1.
If selective-display reaches 0 or below, set it to nil (turn off).
If selective-display is nil, keep it nil."
  (interactive)
  (setq selective-display
        (cond
         ((null selective-display) nil)
         ((numberp selective-display)
          (let ((new-level (1- selective-display)))
            (if (<= new-level 0)
                nil
              new-level)))
         (t nil)))
  (message "Selective display level: %s"
           (if selective-display selective-display "off"))
  ;; Force redisplay
  (redraw-display))

(evil-leader/set-key
  ">" 'my-sd-inc
  "<" 'my-sd-dec)

;; Keybindings
(evil-leader/set-key
  ;; File navigation
  "i" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "o" (lambda () (interactive) (find-file "~/.emacs.d/todo.org"))
  "e" 'my-complete-find-file-force
  "E" 'my-complete-buffers
  "b" 'my-complete-buffer-tags-force
  "<tab>" 'tab-close

  ;; Project management
  "pp" 'my-project-init
  "v" 'my-project-split-right
  "r" 'my-project-replace-under-cursor

  ;; Tags and search
  "t" (lambda () (interactive) (evil-jump-to-tag t))
  "T" 'find-tag
  "xt" 'my-update-tags
  "g" (lambda () (interactive)
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
  "xb" 'eval-buffer
  "n" 'display-line-numbers-mode
  "C-a" 'evil-numbers/inc-at-pt
  "C-x" 'evil-numbers/dec-at-pt
  "q" 'evil-quit

  ;; Version control and diff
  "H" (lambda () (interactive)
        (if (vc-registered (buffer-file-name))
            (progn
              (tab-new)
              (call-interactively 'vc-ediff))
          (message "File is not under version control")))

  ;; Z-specific functions
  "zz" 'z-init
  "zc" 'my-code-review
  "zr" 'z-open-regression
  "si" 'z-sort-imports
  "zi" 'z-get-import
  "c" 'copilot-mode
  "P" 'my-complete-kill-ring
  )

;; Ripgrep
(use-package rg
  :ensure t
  :config
  (define-key rg-mode-map (kbd "]q") 'next-error)
  (define-key rg-mode-map (kbd "[q") 'previous-error))

(define-key evil-insert-state-map (kbd "<S-tab>") (lambda () (interactive) (insert "\t")))

;; Miscellaneous
(defun z-find-kill (beginning end)
  "Take what is in the clipboard and remove matches from file"
  (interactive "r")
  (defalias 'rpex 'replace-regexp-in-string)
  (let ((my-query
         (rpex (read-string "SITE (Enter for \"ALVIN\"): " nil nil "ALVIN") ".*"
               (rpex "\\s-[0-9]+:[0-9]+:[0-9]\\s-" ".*"
                     (rpex "[0-9]+" ".*"
                           (regexp-quote (buffer-substring beginning end)))))))
    (query-replace-regexp my-query "")))

;; Load shell customizations
(load "~/.emacs.d/myshell.el")

;; Custom settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
