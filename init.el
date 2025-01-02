;; vanilla emacs configuration
(add-hook 'before-save-hook 'delete-trailing-whitespace)        ;; delete trailing whitespace on save
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; put a line at 80
(menu-bar-mode -1)
(save-place-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(scroll-bar-mode -1)
(setq echo-keystrokes .1)                                       ;; show the keys I'm pressing
(setq indent-tabs-mode nil)
(setq inhibit-startup-screen 1)
(setq mode-require-final-newline t)                             ;; newline at end of document
(setq ring-bell-function 'ignore)
(setq tab-width 4)                                              ;; use spaces instead of tabs
(setq-default fill-column 80)
(tool-bar-mode -1)                                              ;; remove toolbar

;; winner mode (allowes to restore window configuration)
(winner-mode 1)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defun my-ediff-restore-windows ()
  "Restore the previous window configuration after an ediff session."
  (winner-undo))
(add-hook 'ediff-quit-hook #'my-ediff-restore-windows)

;; fill column indicator
(display-fill-column-indicator-mode)
(setq my-line-indicator-color "misty rose")
(set-face-background 'fill-column-indicator my-line-indicator-color)
(set-face-foreground 'fill-column-indicator my-line-indicator-color)

;; Show the current function name in the header line
(setq which-func-unknown "n/a")
(which-function-mode 1) ;; show the current function
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))

;; Save command history between sessions
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

;; Recent files mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; Adjust the number of recent files to keep
(add-hook 'emacs-startup-hook 'recentf-open-files)

;; don't write lock files
(setq create-lockfiles nil)

;; tags search is case sensitive or not
(setq tags-case-fold-search nil)

;; Don't litter project
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups/")))

;; python specific stuff
;; python
;; (setq python-shell-interpreter "c:/ZogoTech/python")
(add-hook 'python-mode-hook
          '(lambda ()
             (define-abbrev python-mode-abbrev-table "pdb" "breakpoint();")))
(add-hook 'python-mode-hook '(lambda () (abbrev-mode 1)))
(add-hook 'python-mode-hook (lambda () (local-set-key (kbd "TAB") 'indent-relative)))
(add-hook 'python-ts-mode-hook (lambda () (local-set-key (kbd "TAB") 'indent-relative)))


;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Uncomment the following line to refresh package archives occasionally
;; (package-refresh-contents)
(setq package-selected-packages '(evil
                                  evil-leader
                                  evil-commentary
                                  evil-numbers
                                  nyan-mode
                                  undo-fu
                                  flycheck
                                  vertico
                                  rg
                                  copilot
                                  dash
				  exec-path-from-shell
                                  s
                                  editorconfig
                                  f
                                  ))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; mac os shell mode
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; nyan mode
(require 'nyan-mode)
(if (display-graphic-p) (progn (nyan-mode 1)))

;; evil
(require 'evil)
(evil-mode 1)

;; evil-leader
(require 'evil-leader)
(global-evil-leader-mode)

;; evil-commentary
(require 'evil-commentary)
(evil-commentary-mode)

;; flycheck
(require 'flycheck)
(add-hook 'js-mode-hook #'flycheck-mode)
(add-hook 'python-mode-hook #'flycheck-mode)
(add-hook 'python-ts-mode-hook #'flycheck-mode)
;; (setq-default flycheck-python-flake8-executable "h:/src/python/python.exe")
(setq flycheck-checker-error-threshold nil)
(setq flycheck-disabled-checkers
      '(python-mypy python-pylint python-pycompile python-pyright))
(setq flycheck-flake8rc ".flake8")
(setq flycheck-highlighting-mode 'lines)
;; (setq flycheck-javascript-jshint-executable
;;       "C:\\Users\\cg\\scoop\\apps\\nodejs\\current\\bin\\jshint")
;; (setq flycheck-jshintrc "~/cg.dotfiles/jshint.config")
(setq flycheck-locate-config-file-functions '(flycheck-locate-config-file-ancestor-directories))
;; only check on save
(setq flycheck-check-syntax-automatically '(save mode-enabled))
;; (setq flycheck-check-syntax-automatically '(save))
(evil-define-key 'normal 'global (kbd "]a") 'flycheck-next-error)
(evil-define-key 'normal 'global (kbd "[a") 'flycheck-previous-error)
;; error list
(add-to-list 'display-buffer-alist
	     `(,(rx bos "*Flycheck errors*" eos)
	       (display-buffer-reuse-window
		display-buffer-in-side-window)
	       (side            . bottom)
	       (reusable-frames . visible)
	       (window-height   . 0.33)))
(add-hook 'flycheck-after-syntax-check-hook
	  (lambda ()
	    (if flycheck-current-errors
		(flycheck-list-errors)
	      (let ((error-list-buffer (get-buffer "*Flycheck errors*")))
		(when error-list-buffer
		  (delete-windows-on error-list-buffer))))))

;; copilot
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode) ;; enable copilot in all programming modes
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-i") 'copilot-accept-completion)
(setq copilot-indent-offset-warning-disable t
      copilot-enable-predicates '(evil-insert-state-p)
      copilot-max-char 99999
      )

;; vertico for completion
(require 'vertico)
(setq completion-styles '(basic substring partial-completion flex))
(setq completion-ignore-case t)
(vertico-mode 1)

;; my stuff
;; my vars
(setq my-project-root (expand-file-name "."))
(setq my-ctags-languages "")
(setq my-ctags-kinds "")
(setq my-ctags-excludes "--exclude=*.min.js --exclude=3p")

;; completers
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

(defun my-complete-find-init (&optional force-refresh)
  (interactive "P")
  (my-completer-cmd (concat "rg --files " (expand-file-name "~/.emacs.d")) 'find-file "find-init.cache" force-refresh))

(defun my-complete-find-file (&optional force-refresh)
  (interactive "P")
  (my-completer-cmd (concat "rg --files --path-separator / " "\"" my-project-root "\"") 'find-file "find-file.cache" force-refresh))

(defun my-complete-buffer-tags (&optional force-refresh)
  "Complete buffer tags."
  (interactive "P")
  (my-completer-cmd
   (concat "ctags -n --python-kinds=cfmv -f- " buffer-file-name)
   (lambda (line)
     (let ((spl (split-string line "\t")))
       (goto-line (string-to-number (substring (nth 2 spl) 0 -2)))))
   "buffer-tags.cache" force-refresh))

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
  "Complete buffer tags."
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

(defun my-complete-find-file-force ()
  (interactive)
  (my-complete-find-file t))

;; my custom functions
(defun my-open-diff()
  (interactive)
  (let (
        (parts (split-string (thing-at-point 'line) " "))
        )
    (ediff (nth 1 parts) (nth 3 parts))
    ))

(define-minor-mode my-code-review-mode
  "Minor mode for a code review"
  :init-value nil
  :lighter "my-code-review"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<return>") 'my-open-diff)
            map)
  )

(defun my-code-review ()
  "Compare source to pristine copy."
  (interactive) ;; Allows the function to be called interactively
  ;; If a buffer named "code-review" exists, kill it
  (when-let ((existing-buf (get-buffer "code-review")))
    (kill-buffer existing-buf))
  ;; Create a new buffer named "code-review"
  (let (($buf (generate-new-buffer "code-review")))
    ;; Ensure tab-bar-mode is enabled
    (when (not (bound-and-true-p tab-bar-mode))
      (tab-bar-mode 1))
    ;; Create a new tab and switch to it
    (tab-new)
    (switch-to-buffer $buf)
    ;; Set up the buffer with the necessary modes
    (text-mode)             ;; Simple text editing mode
    (my-code-review-mode)    ;; Your custom mode for code review (assumption based on the name)
    (hl-line-mode)          ;; Highlight the current line
    ;; Prompt the user for the left directory and use it as the default for the right directory
    (let* ((left-dir (expand-file-name (directory-file-name
                                        (read-directory-name "left: " my-project-root))))
           (right-dir (expand-file-name (directory-file-name
                                         (read-directory-name "right: " left-dir))))
           (cmd (concat
                 "cd / && "
                 "diff -qr "
                 (shell-quote-argument left-dir)
                 " "
                 (shell-quote-argument right-dir))))
      ;; Print the constructed command string to the Emacs messages buffer
      (message "cmd: %s" cmd)
      ;; Run the shell command and insert its output into the buffer
      (insert (shell-command-to-string cmd)))
    ;; Return the buffer object representing the new buffer
    $buf))

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
      (let (
	    (selection
	     (buffer-substring-no-properties (region-beginning) (region-end))))
	(if (= (length selection) 0)
	    (message "empty string")
	  (evil-ex (concat "%s/" selection "/"))
	  ))
    (evil-ex (concat "%s/" (thing-at-point 'word) "/"))))

;; keybindings
;; set keybindings with default leader
(evil-leader/set-key
  "i" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))	;; open init file
  "xb" 'eval-buffer							;; run current buffer
  "v" 'evil-window-vsplit						;; split window right
  "r" 'my-project-replace-under-cursor				        ;; replace under cursor
  "e" 'my-complete-find-file                                            ;; find files
  "pe" 'my-complete-find-file                                            ;; find files
  "b" 'my-complete-buffer-tags                                          ;; complete buffer tags
  "<tab>" 'tab-close
  "q" 'evil-quit
  ;; ask user for directory to set as project root, use current project root as default strip the trailing slash and expand the path in case of tilde
  "pp" (lambda () (interactive) (setq my-project-root (expand-file-name (directory-file-name (read-directory-name "Project root: " my-project-root)))))
  )

(evil-define-key 'normal 'global (kbd "] <tab>") 'tab-next)
(evil-define-key 'normal 'global (kbd "[ <tab>") 'tab-previous)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types
   '((copilot copilot-exceeds-max-char)
     (copilot copilot-exceeds-max-char))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
