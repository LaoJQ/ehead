;;; ahead.el --- jump to define of record or macro in erlang source file
;;
;; deps:
;;  erlang


(defvar ehead-searched-sets nil
  "To avoid endless loop by hrl file circular reference,
when have searched a hrl file, lookup if in this sets or not,
and push itself into it when search nil. It will be clear at the
end of every ehead-jump invoke.")


(defvar ehead-jump-ring (make-ring 20)
  "Store the definition jump history")


(defvar ehead-jump-other-kind-interface nil
  "It can provide a function for ehead-jump so that
it can jump to function, type or other kind of name.")


(defun ehead-jump ()
  "Jump to the definition of record or macro at current point"
  (interactive)
  (let* ((id (ehead-get-identifier-at-point))
         (kind (car id))
         (name (nth 1 id)))
    (cond ((eq kind 'record)
           (ehead-jump-to-definition "^-record(\\s-*%s\\s-*," name))
          ((eq kind 'macro)
           (ehead-jump-to-definition "^-define(\\s-*%s\\s-*[,(]" name))
          (t
           (unless (ehead-find-hrl-at-point)
             (if ehead-jump-other-kind-interface
                 (funcall ehead-jump-other-kind-interface)
               (message "EHEAD EARN: %s is not record or macro." name)))))))


(defun ehead-get-identifier-at-point ()
  "Get the identifier at current point. Return:
('record \"NAME\") --- is a record
('macro \"NAME\")  --- is a macro
(nil \"NAME\")     --- neither nor"
  (let* ((id (erlang-get-identifier-at-point))
         (kind (erlang-id-kind id))
         (name (erlang-id-name id)))
    (if (or (eq kind 'record) (eq kind 'macro))
        (list kind name)
      (list nil name))))


(defun ehead-jump-to-definition (str name)
  "Jump to the definition of NAME when found. STR is the raw regexp."
  (let* ((re (format str name))
         (found (ehead-search-definition-with-buffer (current-buffer) re))
         result)
    (if found
        (progn
          (ehead-add-to-ring)
          (switch-to-buffer (marker-buffer found))
          (goto-char (marker-position found))
          (setq result (search-backward name))
          (recenter-top-bottom))
      (message "EHEAD EARN: %s is not found." name))
    (setq ehead-searched-sets nil)
    result))


(defun ehead-add-to-ring ()
  "Found and add origin point-marker to ring"
  (ring-insert-at-beginning ehead-jump-ring (point-marker)))


(defun ehead-search-definition-with-buffer (buffer re)
  "Search to match RE in the given buffer BUFFER, and return the marker of the
definition. Otherwise, return nil."
  (let* ((file (buffer-file-name buffer))
         found hrl-stack hrl hrl-buffer)
    (unless (member file ehead-searched-sets)
        (save-excursion
          (with-current-buffer buffer
            (goto-char (point-min))
            (setq found (and (re-search-forward re nil t) (point-marker)))
            (unless found
              (progn (push file ehead-searched-sets)
                     (setq hrl-stack (ehead-find-all-include))
                     (while (and (not found) (setq hrl (pop hrl-stack)))
                       (if (setq hrl-buffer (find-buffer-visiting hrl))
                           (setq found (ehead-search-definition-with-buffer hrl-buffer re))
                         (progn (setq found (ehead-search-definition-with-buffer (setq hrl-buffer (find-file-noselect hrl)) re))
                                (unless found
                                  (kill-buffer hrl-buffer))
                                found))))))))
    found))


(defun ehead-find-all-include ()
  "Find all include file in current buffer, and push to list which will be return."
  (let* ((project-path (ehead-project-root-path))
         hrl hrl-path stack)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^-include(\\s-*\"\\(.+?\\)\"\\s-*)" nil t)
        (setq hrl (match-string-no-properties 1))
        (cond ((and project-path (file-exists-p (setq hrl-path (concat project-path "include/" hrl))))
               (push hrl-path stack))
              ((file-exists-p (setq hrl-path (expand-file-name hrl)))
               (push hrl-path stack)))))
    stack))


(defun ehead-project-root-path ()
  "Find the erlang project root path and return path string.
If not found rebar.config or .git, return nil."
  (let* (project-path)
    (if (setq project-path (or (locate-dominating-file default-directory "rebar.config")
                               (locate-dominating-file default-directory ".git")))
        (expand-file-name project-path)
      nil)))


(defun ehead-back ()
  "Jump back."
  (interactive)
  (unless (ring-empty-p ehead-jump-ring)
    (let* ((marker (ring-remove ehead-jump-ring))
           (buffer (marker-buffer marker)))
      (if (buffer-live-p buffer)
          (progn (switch-to-buffer buffer)
                 (goto-char (marker-position marker)))
        ;; TODO: ring store file path for reopen when buffer was killed
        (ehead-back)))))


(defun ehead-find-hrl-at-point ()
  "Jump to hrl file which at point."
  (save-excursion
    (move-end-of-line 1)
    (let* ((project-path (ehead-project-root-path))
           (bound (point))
           hrl hrl-path)
      (move-beginning-of-line 1)
      (if (re-search-forward "^-include(\\s-*\"\\(.+?\\)\"\\s-*)" bound t)
          (progn (setq hrl (match-string-no-properties 1))
                 (cond ((and project-path (file-exists-p (setq hrl-path (concat project-path "include/" hrl))))
                        (ehead-find-hrl-at-point-goto hrl-path))
                       ((file-exists-p (setq hrl-path (expand-file-name hrl)))
                        (ehead-find-hrl-at-point-goto hrl-path))
                       (t
                        nil)))
        nil)
      )))

(defun ehead-find-hrl-at-point-goto (hrl-path)
  "Jump to file and add origin to ring."
  (progn (ehead-add-to-ring)
         (find-file hrl-path)))



;; About grep something in erlang project path
(defun ehead-grep-mark (subdir start end)
  "Read mark and input a subdir in current erlang project to call function grep.
default sub dir is src/"
  (interactive
   (list (read-string "(project-grep) Sub Dir(default src/): " nil nil "src/")
         (region-beginning)
         (region-end)))
  (let* ((pattern (buffer-substring start end)))
    (ehead-grep pattern subdir)))


(defun ehead-grep-input (pattern subdir)
  "Input a pattern and subdir in current erlang project to call function grep.
default sub dir is src/"
  (interactive
   (list (read-string "(project-grep) Pattern: ")
         (read-string "(project-grep) Sub Dir(default src/): " nil nil "src/")))
  (ehead-grep pattern subdir))


(defun ehead-grep (pattern subdir)
  "Grep pattern in erlang project path or current path."
  (let ((project-path (ehead-project-root-path)))
    (save-excursion
      (if project-path
          (set-buffer (dired-noselect project-path))
        (setq subdir "./"))
      (grep (concat "grep --color -nH -re \"" pattern "\" " subdir))))
  )



(provide 'ehead)
