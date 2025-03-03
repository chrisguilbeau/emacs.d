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

;; treesitter stuff
;; (treesit-install-language-grammar) ; do this to install a language
;; remap modes to ts-mode
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
;; create a dynamic hook based on if ts is installed
(setq my-python-hook
      (if (treesit-available-p) 'python-ts-mode-hook 'python-mode-hook))

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-min-dir-content 1
      uniquify-strip-common-suffix t)
;; set the format to put the dir name first
(setq uniquify-buffer-name-style 'forward)

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
;; (add-hook 'emacs-startup-hook 'recentf-open-files)

;; don't write lock files
(setq create-lockfiles nil)

;; tags search is case sensitive or not
(setq tags-case-fold-search nil)

;; Don't litter project
;; create if doesn't exist
(unless (file-directory-p "~/.emacs.d/autosaves")
  (make-directory "~/.emacs.d/autosaves"))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups/")))

;; python specific stuff
(add-hook my-python-hook (lambda () (local-set-key (kbd "TAB") 'indent-relative)))
(add-hook 'python-ts-mode-hook (lambda () (local-set-key (kbd "TAB") 'indent-relative)))
(add-hook my-python-hook
	  (lambda ()
	    ;; set some evil leader bindings
	    (evil-leader/set-key
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
                                  flycheck
                                  ripgrep
                                  vertico
                                  rg
                                  copilot
				  copilot-chat
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

;; tab-bar-mode
;; customize the colors
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
(set-face-attribute 'tab-bar-tab nil :box nil)
;; set the font to be the same
(tab-bar-mode 1)
(setq tab-bar-show 1)

;; mac os shell mode
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; nyan mode
(require 'nyan-mode)
(if (display-graphic-p) (progn (nyan-mode 1)))

;; evil
(require 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)

;; evil-leader
(require 'evil-leader)
(global-evil-leader-mode)

;; evil-commentary
(require 'evil-commentary)
(evil-commentary-mode)

;; flycheck
(require 'flycheck)
(add-hook 'js-mode-hook #'flycheck-mode)
(add-hook my-python-hook #'flycheck-mode)
;; only on windows hook
(add-hook 'python-mode-hook
	  (lambda ()
	    (when (eq system-type 'windows-nt)
	      (flycheck-mode 1)
	      (setq-default flycheck-python-flake8-executable
			    "h:/src/python/python.exe")
	      (setq flycheck-jshintrc "C:/ZogoTech/hg/cg.dotfiles/jshint.config")
	      )))
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(python-pyright python-pylint python-mypy python-pycompile))
  )
(setq flycheck-checker-error-threshold nil)
;; (setq flycheck-flake8rc ".flake8")
(setq flycheck-highlighting-mode 'lines)
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
(add-to-list 'load-path "~/.emacs.d/copilot.el")
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
(setq my-project-prist my-project-root)
(setq my-ctags-languages "")
(setq my-ctags-kinds "")
(setq my-ctags-excludes "")


(defun my-project-init()
  (interactive)
  (setq my-project-root (expand-file-name (directory-file-name (read-directory-name "Project root: " my-project-root))))
  (setq my-project-prist my-project-root)
  (setq my-complete-find-file-command "rg --files --path-separator / -tpy -tjs -tcss -ttxt ")
  (setq my-diff-command "diff -qr -x .hg -x .git -x etags")
  ;; set the window title to the project root
  (setq frame-title-format my-project-root)
  (cd my-project-root)
  (find-file my-project-root)
  (message "project initialized")
  )

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

(setq my-complete-find-file-command "rg --files --path-separator / ")

(defun my-complete-find-file (&optional force-refresh)
  (interactive "P")
  (my-completer-cmd (concat my-complete-find-file-command "\"" my-project-root "\"") 'find-file "find-file.cache" force-refresh))

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

(defun my-complete-project-tags-force()
  (interactive)
    (my-complete-project-tags t))

(defun my-complete-buffer-tags-force ()
  (interactive)
  (my-complete-buffer-tags t))

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

(setq my-diff-command "diff -qr ")

(defun my-code-review ()
  "Compare source to pristine copy."
  (interactive) ;; Allows the function to be called interactively
  ;; If a buffer named "code-review" exists, kill it
  (when-let ((existing-buf (get-buffer "code-review")))
    (kill-buffer existing-buf))
  ;; Create a new buffer named "code-review"
  (let (($buf (generate-new-buffer "code-review")))
    ;; Ensure tab-bar-mode is enabled
    ;; (when (not (bound-and-true-p tab-bar-mode))
    ;;   (tab-bar-mode 1))
    ;; Create a new tab and switch to it
    (tab-new)
    (switch-to-buffer $buf)
    ;; Set up the buffer with the necessary modes
    (text-mode)             ;; Simple text editing mode
    (my-code-review-mode)    ;; Your custom mode for code review (assumption based on the name)
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

(defun get-my-ctags-command()
  (concat "ctags -Ref " my-project-root "/etags " my-project-root))

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

;; keybindings
;; set keybindings with default leader
(evil-leader/set-key
  "i" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))	;; open init file
  "xb" 'eval-buffer							;; run current buffer
  "xt" (lambda () (interactive)
	 (message (get-my-ctags-command))
	 (shell-command (get-my-ctags-command))
	 (visit-tags-table (concat my-project-root "/etags"))
	 ) ;; update tags
  "t" (lambda () (interactive) (evil-jump-to-tag t))
  "T" 'find-tag
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
  ;; toggle line numbers on and off not relative
  "n" 'display-line-numbers-mode
  "v" 'my-project-split-right						;; split right
  "r" 'my-project-replace-under-cursor				        ;; replace under cursor
  "e" 'my-complete-find-file-force                                      ;; find files
  "b" 'my-complete-buffer-tags-force                                    ;; complete buffer tags
  "<tab>" 'tab-close
  "C-a" 'evil-numbers/inc-at-pt
  "C-x" 'evil-numbers/dec-at-pt
  "c" 'copilot-mode
  "H" (lambda () (interactive)
	(if (vc-registered (buffer-file-name))
	    (progn
	      (tab-new)
	      (call-interactively 'vc-ediff))
	  (message "File is not under version control")))
  "o" (lambda () (interactive) (find-file "~/.emacs.d/todo.org"))
  "q" 'evil-quit
  ;; ask user for directory to set as project root, use current project root as default strip the trailing slash and expand the path in case of tilde
  "pp" 'my-project-init
  )

(evil-define-key 'normal 'global (kbd "] <tab>") 'tab-next)
(evil-define-key 'normal 'global (kbd "[ <tab>") 'tab-previous)

;; fake quick-fix
(evil-define-key 'normal 'global (kbd "]q") 'next-error)
(evil-define-key 'normal 'global (kbd "[q") 'previous-error)
(require 'rg)
(define-key rg-mode-map (kbd "]q") 'next-error)
(define-key rg-mode-map (kbd "[q") 'previous-error)

;; Mac only stuff
(when (eq system-type 'darwin)
  (set-frame-font "Comic Code-13")
  (set-face-attribute 'tab-bar nil :font "Comic Code-14")
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
    )
  )

;; windows only stuff
(when (eq system-type 'windows-nt)
  ;; set font to consolas
  (set-frame-font "Comic Code-10")
  (set-face-attribute 'tab-bar nil :font "Comic Code-10")
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
  (defun z-init()
    (interactive)
    (setq my-dev (get-junction-target "h:/dev"))
    (visit-tags-table (concat my-dev "/etags"))
    (setq my-project-root (get-junction-target "h:/dev"))
    (setq my-project-prist (get-junction-target "h:/prist"))
    (setq my-complete-find-file-command "rg --files --path-separator / -tpy -tjs -tcss -ttxt ")
    (setq my-diff-command "diff -qr -x .hg -x *.pyc -x __pycache__ -x zebra -x etags -x nvim-undo -x vim-undo -x tags ")
    ;; set the window title to the project root
    (setq frame-title-format my-project-root)
    (message "the z has been initted for windows...")
    )
  (defun z-open-regression ()
    "Open the regression folder, refresh its contents and sort files by time."
    (interactive)
    ;; Ensure tab-bar-mode is enabled
    ;; (when (not (bound-and-true-p tab-bar-mode))
    ;;   (tab-bar-mode 1))
    ;; Create a new tab
    (tab-new)
    ;; Open the regression directory in Dired mode in the new tab
    (dired "\\\\regr3.ztaustin.local\\home\\cg\\regression-output")
    ;; Refresh Dired buffer
    (revert-buffer)
    ;; Sort files by time if in 'Dired by name' mode
    ;; (when (equal mode-name "Dired by name")
    ;;   (dired-sort-toggle-or-edit))
    ;; return should open file in a new window not replace the buffer
    (define-key dired-mode-map (kbd "RET") 'dired-find-file-other-window)
    )
  )

(defun z-find-kill (beginning end)
  "take what is in the clipboard and remove matches from file"
  (interactive "r")
  (defalias 'rpex 'replace-regexp-in-string)
  (let ( ;; set local vars
	(my-query
	 (rpex (read-string "SITE (Enter for \"ALVIN\"): " nil nil "ALVIN") ".*"  ;; site
	       (rpex "\s[0-9]+:[0-9]+:[0-9]\s" ".*"
		     (rpex "[0-9]+" ".*"  ;; numbers
			   (regexp-quote (buffer-substring beginning end)))))))
    (query-replace-regexp my-query "")
    ))

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
  "insert an import that is already used in your project"
  (interactive)
  ;; move cursor to the beginning of the line
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

;; (fmakunbound 'xref-find-definitions)
;; set tmp to the function slot #'xref-find-definitions
;; do the thing
;; set the function slot back to the original

(evil-leader/set-key
  "zz" 'z-init
  "zc" 'my-code-review
  "zr" 'z-open-regression
  "si" 'z-sort-imports
  "zi" 'z-get-import
  )

(defun my-shell-setup()
  (interactive)
  (load "~/.emacs.d/myshell.el")
  (load "~/.emacs.d/mypdbtrack.el")
  (add-hook 'prog-mode-hook (flycheck-mode nil))
  )
(defun my-debugger-setup()
  (interactive)
  ;; Prevent Emacs from raising its frame when the server opens a file
  (global-flycheck-mode 0)
  ;; turn on line numbers no matter what
  (display-line-numbers-mode 1)
  (setq server-raise-frame nil)
  (server-start)
  )

;; EMACS CUSTOMIZATION STUFF BELOW
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(copilot-chat-model "claude-3.7-sonnet")
 '(warning-suppress-log-types
   '((copilot copilot-exceeds-max-char) (copilot copilot-exceeds-max-char)))
 '(xref-show-definitions-function 'xref-show-definitions-completing-read))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
