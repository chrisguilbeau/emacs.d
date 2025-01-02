(defun z-project-init()
  "init zogotech"
  (interactive)
  (setq my-project-root "h:/dev")
  (setq default-directory my-project-root)
  (message default-directory)
  (setq my-project-prist "h:/prist")
  (setq my-project-folders "appserver server webserver support")
  (setq my-ctags-bin "ctags")
  (setq my-ctags-excludes "--exclude=.hg --exclude=*.min.js --exclude=*.min.css --exclude=3p")
  (setq my-ctags-languages "--languages=Python,Javascript,CSS")
  (setq my-ctags-kinds "--python-kinds=cfmvl")
  (setq my-ctags-file-name "etags")
  (setq my-rg-bin "rg")
  (setq my-rg-types "-tpy -tjs -tcss -ttxt")
  (setq python-shell-interpreter "h:/src/python/python.exe")
  (setq-default flycheck-python-flake8-executable "h:/src/python/python.exe")
  (evil-leader/set-key
    "zr" 'z-open-regression
    "zk" 'z-find-kill
    "zc" 'my-code-review
    "zi" 'z-get-import
    "si" 'z-sort-imports
    "zg" '(lambda () (interactive)
	    (tab-new)
	    ;; run ripgrep regexp, promot for word, default to word at point and run on the current directory with autocomplete
	    ;; also tell ripgrep to ignore tags and etags files
	    (ripgrep-regexp (read-string "Find: " (thing-at-point 'word))
			    (read-directory-name "Directory: " default-directory)
			    ;; pass args as sequence of strings
			    (list "-g!tags"
				  "-g!etags"
				  )))
    )
  (message (z-windows-resolve-junction my-project-root))
  ;; (setq frame-title-format (my-win-resolve-junction my-project-root))
  (setq frame-title-format (string-trim (shell-command-to-string "z_get_proj.py")))
  (setq my-project-ctags-file (concat my-project-root "/" my-ctags-file-name))
  (visit-tags-table my-project-ctags-file)
  )

(defun z-windows-resolve-junction (dir)
  "Return the target directory for the Windows junction at DIR.
Uses `fsutil reparsepoint query DIR` and parses out the 'Substitute Name'."
  (interactive "DDirectory: ")
  (let* ((dir  (directory-file-name (expand-file-name dir)))
         (temp (generate-new-buffer "*fsutil-output*")))
    (unwind-protect
        (progn
          (unless (zerop (call-process "fsutil" nil temp nil
                                       "reparsepoint" "query" dir))
            (error "Failed to query reparse point.  Is `%s` a junction?" dir))
          (with-current-buffer temp
            (goto-char (point-min))
            (if (re-search-forward "^Substitute Name:\\s-*\\(.*\\)$" nil t)
                (let ((target (match-string 1)))
                  ;; Often starts with \??\ — remove that if present:
                  (when (string-match "^\\\\\\?\\\\?" target)
                    (setq target (substring target (match-end 0))))
                  (if (called-interactively-p 'interactive)
                      (message "Target: %s" target)
                    target))
              (error "No 'Substitute Name' found in fsutil output."))))
      (kill-buffer temp))))

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

(defun z-get-import()
  "insert an import that is already used in your project"
  (interactive)
  (let (
	(file-lines
	 (split-string
	  (shell-command-to-string (concat "cd " my-project-root
					   " && "
					   my-rg-bin " --follow "
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
      (forward-line -1)
      (z-sort-imports)
      )
    ))

(defun z-open-regression ()
  "Open the regression folder, refresh its contents and sort files by time."
  (interactive)
  ;; Ensure tab-bar-mode is enabled
  (when (not (bound-and-true-p tab-bar-mode))
    (tab-bar-mode 1))
  ;; Create a new tab
  (tab-new)
  ;; Open the regression directory in Dired mode in the new tab
  (dired "\\\\regr3.ztaustin.local\\home\\cg\\regression-output")
  ;; Refresh Dired buffer
  (revert-buffer)
  ;; Sort files by time if in 'Dired by name' mode
  (when (equal mode-name "Dired by name")
    (dired-sort-toggle-or-edit)))

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
