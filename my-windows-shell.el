;; SHELL SECTION
;; make shell paths clickable
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'python-traceback)
(add-to-list 'compilation-error-regexp-alist-alist
             '(python-traceback
               "^  File \"\\(.*?\\)\", line \\([0-9]+\\)," 1 2))
(setenv "PYTHONUNBUFFERED" "1")
(setq comint-prompt-read-only t)
(defun go-to-end-of-prompt ()
  "Move cursor to end of prompt when entering insert mode."
  (when (derived-mode-p 'comint-mode)
    (goto-char (point-max))))
(add-hook 'evil-insert-state-entry-hook 'go-to-end-of-prompt)
(with-eval-after-load 'evil
  (evil-define-key 'insert comint-mode-map (kbd "<up>") 'comint-previous-input)
  (evil-define-key 'insert comint-mode-map (kbd "<down>") 'comint-next-input)
  )
(defun switch-to-evil-state-on-click ()
  (interactive)
  (evil-normal-state))
(add-hook 'shell-mode-hook
	  (lambda ()
	    (evil-leader/set-key
	      "l" 'comint-clear-buffer
	      "~" 'restart-shell)
	    ;; load ~/.emacs.d/myfiles/mypdbtrack.el
	    (load-file (expand-file-name "~/.emacs.d/myfiles/mypdbtrack.el"))
	    (scroll-bar-mode)
	    (define-key shell-mode-map [down-mouse-1] 'switch-to-evil-state-on-click)))
;; (defun switch-to-evil-normal-state-on-click (event)
;;   "Switch to evil normal state on mouse click."
;;   (interactive "e")
;;   (when (evil-insert-state-p)
;;     (evil-normal-state)))
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (define-key shell-mode-map [down-mouse-1] 'switch-to-evil-normal-state-on-click)))
(defun shell-frame ()
  "Focus the shell frame. If it doesn't exist, create one and open a shell."
  (interactive)
  (let* ((shell-frame-name "Shell-Frame")
         (frame (seq-find (lambda (f) (string= (frame-parameter f 'name) shell-frame-name))
                          (frame-list))))
    ;; If frame exists, just select it
    (if frame
        (select-frame-set-input-focus frame)
      ;; If frame does not exist, create it and then start a shell inside
      (let ((new-frame (make-frame `((name . ,shell-frame-name)))))
        (select-frame-set-input-focus new-frame)
        (shell (generate-new-buffer-name "*shell*"))))))
(defun restart-shell ()
  "Kill the current buffer (which should be a shell) and start a new
    shell with the same name in the same directory."
  ;; Make the function interactive, allowing it to be called with M-x
  (interactive)
  ;; Check if the major mode of the current buffer is not shell-mode.
  ;; If it's not, an error is raised notifying the user that the current buffer is not a shell
  (if (not (eq major-mode 'shell-mode))
      (error "Current buffer is not a shell")
    ;; If the current buffer is indeed a shell-mode buffer,
    ;; store the current buffer name and default directory in
    ;; local variables
    (let ((name (buffer-name))
	  (current-directory default-directory))
      ;; Kill the current buffer
      (kill-buffer)
      ;; Using a nested let to keep the default-directory value as the
      ;; current directory (which we stored before killing the buffer)
      ;; and start a new shell with the same name
      (let ((default-directory current-directory))
	(shell name)))))
;; track my directory based on prompt
(defun my-update-directory-tracker (output)
  "Update the current directory based on the shell OUTPUT."
  (when (string-match "\\(.*\\)>" output)
    (let ((path (match-string 1 output)))
      (when (file-directory-p path)
        (cd path)))))
(defun my-shell-mode-hook ()
  "Shell mode customizations."
  (setq-local comint-prompt-regexp ".*\n>")
  (add-hook
   'comint-output-filter-functions
   'my-update-directory-tracker nil t))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(defun my-custom-compilation-regex ()
  "Set up custom regex for matching Windows file paths in compilation buffer."
  (interactive)
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(windows-file-path
     "^[ \t]*\\([a-zA-Z]:\\\\[^:(]+\\)(\\([0-9]+\\))"
     1 2))
  (add-to-list 'compilation-error-regexp-alist 'windows-file-path))
(add-hook 'compilation-mode-hook 'my-custom-compilation-regex)
(add-hook 'compilation-shell-minor-mode-hook 'my-custom-compilation-regex)
