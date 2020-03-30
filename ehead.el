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


(defvar ehead-erlang-root-path nil
  "Erlang install path")


(defconst ehead-regex-include "^-include(\\s-*\"\\(.+?\\)\"\\s-*)")
(defconst ehead-regex-include-lib "^-include_lib(\\s-*\"\\(.+?\\)\"\\s-*)")


(defun ehead-jump ()
  "Jump to the definition of record or macro at current point"
  (interactive)
  (let* ((id (ehead-get-identifier-at-point))
         (kind (erlang-id-kind id))
         (module (erlang-id-module id))
         (name (erlang-id-name id))
         (arity (erlang-id-arity id)))
    (cond ((eq kind 'record)
           (ehead-jump-to-definition "^-record(\\s-*%s\\s-*," name))
          ((eq kind 'macro)
           (ehead-jump-to-definition "^-define(\\s-*%s\\s-*[,(]" name))
          ((eq kind 'qualified-function)
           (ehead-jump-to-function-definition module name arity))
          ((ehead-find-hrl-at-point) ;; TODO set id for -include
           )
          (t
           (message "EHEAD WARN: Invalid identifier at point.")))))


(defun ehead-get-identifier-at-point ()
  "Get the identifier at current point. The struct is same as the return of erlang-get-identifier-at-point."
  (let* ((id (erlang-get-identifier-at-point))
         (kind (erlang-id-kind id))
         (module (erlang-id-module id))
         (name (erlang-id-name id))
         (arity (erlang-id-arity id)))
    (cond ((and module name (not arity) (not kind))
           (if (setq arity (ehead-get-slash-arity))
               (list 'qualified-function module name arity)
             id))
          ((and name arity (not kind))
           (list 'qualified-function module name arity))
          (t id))))


(defun ehead-get-slash-arity ()
  "Get the number of arity which match 'f/a' or 'm:f/a'."
  (let* (end-point
         a begin-point)
    (save-excursion
      (re-search-forward "[,\n]")
      (setq end-point (point)))
    (save-excursion
      (looking-at "/")
      (forward-char)
      (setq begin-point (point))
      (re-search-forward "[0-9]+" end-point t)
      (setq a (match-string-no-properties 0))
      (if (equal begin-point (point))
          nil
        (string-to-number a)))))


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
      ;; normal include
      (goto-char (point-min))
      (while (re-search-forward ehead-regex-include nil t)
        (setq hrl (match-string-no-properties 1))
        (cond ((and project-path (file-exists-p (setq hrl-path (concat project-path "include/" hrl))))
               (push hrl-path stack))
              ((file-exists-p (setq hrl-path (expand-file-name hrl)))
               (push hrl-path stack))))
      ;; lib include
      (goto-char (point-min))
      (while (re-search-forward ehead-regex-include-lib nil t)
        (setq hrl (match-string-no-properties 1))
        (cond ((setq hrl-path (ehead-check-standard-hrl-lib hrl ehead-erlang-root-path))
               (push hrl-path stack))
              ((setq hrl-path (ehead-check-deps-hrl-lib hrl project-path))
               (push hrl-path stack))))
      )
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
           hrl hrl-path
           hrl-name hrl-lib)
      (move-beginning-of-line 1)

      (cond
       ;; normal include
       ((re-search-forward ehead-regex-include bound t)
        (setq hrl (match-string-no-properties 1))
        (cond ((and project-path (file-exists-p (setq hrl-path (concat project-path "include/" hrl))))
               (ehead-find-hrl-at-point-goto hrl-path))
              ((file-exists-p (setq hrl-path (expand-file-name hrl)))
               (ehead-find-hrl-at-point-goto hrl-path))
              (t
               nil)))
       ;; lib include
       ((re-search-forward ehead-regex-include-lib bound t)
        (setq hrl (match-string-no-properties 1))
        (cond
         ;; app deps lib
         ((setq hrl-path (ehead-check-deps-hrl-lib hrl project-path))
          (ehead-find-hrl-at-point-goto hrl-path))
         ;; erlang standard lib
         ((setq hrl-path (ehead-check-standard-hrl-lib hrl ehead-erlang-root-path))
          (ehead-find-hrl-at-point-goto hrl-path))
         ;; not match
         (t nil)))
       (t nil))
      )))

(defun ehead-check-standard-hrl-lib (hrl erlang-root)
  "Get hrl file which is matched in '-include(*).' abs path."
  (let* (hrl-path hrl-lib hrl-lib-other hrl-lib-path)
    (if (string-match "\\(.+?\\)/\\(.+\\)" hrl)
        (progn (setq hrl-lib (match-string 1 hrl))
               (setq hrl-lib-other (match-string 2 hrl)))
      nil)
    (if (setq hrl-lib-path (car (split-string (shell-command-to-string (concat "find " erlang-root "/lib -type d -name '" hrl-lib "*'")))))
        (setq hrl-path (concat hrl-lib-path "/" hrl-lib-other))
      nil)
    (if (and hrl-path (file-exists-p hrl-path))
        hrl-path
      nil)
    ))

(defun ehead-check-deps-hrl-lib (hrl project-path)
  "Get hrl file which is matched in '-include_lib(*).' abs path."
  (let* (hrl-path hrl-name)
    (setq hrl-name (if (string-match ".*/\\(.+?\\)$" hrl)
                       (match-string 1 hrl)
                     nil))
    (if hrl-name
        (let* ((ms (split-string (shell-command-to-string (concat "find " project-path " -type f -name " hrl-name))))
               found)
          (while (and (not found) (setq hrl-path (pop ms)))
            (when (string-match hrl hrl-path)
              (setq found t))
            )
          (if found
              hrl-path
            nil))
      nil)))


(defun ehead-find-hrl-at-point-goto (hrl-path)
  "Jump to file and add origin to ring."
  (progn (ehead-add-to-ring)
         (find-file hrl-path)))



(defun ehead-test ()
  ""
  (interactive)
  (let* ((a (erl-read-call-mfa))
         (b (erlang-get-identifier-at-point))
         (c (ehead-get-identifier-at-point)))
    (print a)
    (print b)
    (print c))
  )


(defun ehead-jump-to-function-definition (m f a)
  "Jump to definition of function"
  (interactive)
  (progn
    (ehead-add-to-ring)
    (if (or (eq m (erlang-get-module))
            (eq m nil)) ;; TODO import | bif
        (when f
          (ehead-search-function f a))
      (let* ((erlang-root ehead-erlang-root-path)
             (project-path (ehead-project-root-path))
             erl-path)
        (cond ((setq erl-path (car (split-string (shell-command-to-string (concat "find " erlang-root "/lib -type f -name '" m ".erl'")))))
               (find-file erl-path)
               (ehead-search-function f a))
              ((setq erl-path (car (split-string (shell-command-to-string (concat "find " project-path " -type f -name '" m ".erl'")))))
               (find-file erl-path)
               (ehead-search-function f a))
              (t
               (message "EHEAD EARN: Not found %s:%s/%d" m f a)))))))


;; TODO independence without distel
(defun ehead-search-function (f a)
  ""
  (and (erl-search-definition f a)
       (erl-flash-region)))



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
