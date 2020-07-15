;;; ahead.el --- Ehead is a plugins for Erlang code program in emacs.
;;
;; features:
;; + Jump to definition of function.
;; + Jump to record or macro.
;; + Jump to include file.
;; + Get function reference.
;; + Grep code in project.
;; + Auto completion.
;; + Compile project by reabr3.
;; + Fuzzy find module in project and standard lib.
;; + Needn't extra erlang node for code navigation.
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


(defvar ehead-erlang-root-lib-path nil
  "Erlang install path")


(defconst ehead-regex-include "^-include(\\s-*\"\\(.+?\\)\"\\s-*)")
(defconst ehead-regex-include-lib "^-include_lib(\\s-*\"\\(.+?\\)\"\\s-*)")


(defvar ehead-user-porject-list nil
  "It's a list, to cache the User's projetc root path.")


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
          ((or (eq kind 'include) (eq kind 'include-lib))
           (ehead-find-include-at-point-jump kind name))
          ((or (eq kind 'keyword) (eq kind 'operator) (eq kind 'guard))
           (message "EHEAD LOG: %s %s" kind name))
          (t
           (message "EHEAD WARN: Invalid identifier at point.")))))


(defun ehead-get-identifier-at-point ()
  "Get the identifier at current point. The struct is same as the return of erlang-get-identifier-at-point."
  (or (ehead-find-include-at-point-id)
      (let* ((id (erlang-get-identifier-at-point))
             (kind (erlang-id-kind id))
             (module (erlang-id-module id))
             (name (erlang-id-name id))
             (arity (erlang-id-arity id)))
        (cond ((and module name (not arity) (not kind))
               (cond ((setq arity (ehead-get-slash-arity))
                      (list 'qualified-function module name arity))
                     ((member name erlang-keywords)
                      (list 'keyword nil name nil))
                     ((member name erlang-operators)
                      (list 'operator nil name nil))
                     ((member name erlang-guards)
                      (list 'guard nil name nil))
                     (t
                      id)))
              ((and name arity (not kind))
               (list 'qualified-function module name arity))
              (t id)))))


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
          (recenter-top-bottom)
          (ehead-found-flash-region))
      (message "EHEAD WARN: Not found symbol %s." name))
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
                         (progn (setq found (ehead-search-definition-with-buffer (setq hrl-buffer (find-file-noselect hrl t)) re))
                                (unless found
                                  (kill-buffer hrl-buffer))
                                found))))))))
    found))


(defun ehead-find-all-include ()
  "Find all include file in current buffer, and push to list which will be return."
  (let* ((main-path (ehead-root-path-main-project))
         (app-path (ehead-root-path-sub-app))
         hrl hrl-path stack)
    (save-excursion
      ;; normal include
      (goto-char (point-min))
      (while (re-search-forward ehead-regex-include nil t)
        (setq hrl (match-string-no-properties 1))
        (cond ((file-exists-p (setq hrl-path (expand-file-name (concat "include/" hrl) app-path)))
               (push hrl-path stack))
              ((file-exists-p (setq hrl-path (expand-file-name hrl)))
               (push hrl-path stack))))
      ;; lib include
      (goto-char (point-min))
      (while (re-search-forward ehead-regex-include-lib nil t)
        (setq hrl (match-string-no-properties 1))
        (cond ((setq hrl-path (ehead-check-standard-hrl-lib hrl))
               (push hrl-path stack))
              ((setq hrl-path (ehead-check-deps-hrl-lib hrl main-path))
               (push hrl-path stack))))
      )
    stack))


(defun ehead-root-path-sub-app ()
  "Return the root path of one dep app, or one std app. (Used for find hrl file with the form of '-include(*).')"
  (let* ((cur-path (expand-file-name default-directory)))
    (if (string-match (expand-file-name "[^/]*/" ehead-erlang-root-lib-path) cur-path)
        (match-string 0 cur-path)
      (ehead-search-raw-app-root-path cur-path))))

(defun ehead-root-path-main-project ()
  "Return the user or std project root path. (Used for other case except for 'ehead-root-path-sub-app' say.)"
  (let* ((cur-path (expand-file-name default-directory))
         root-path)
    (if (string-match-p ehead-erlang-root-lib-path cur-path)
        ehead-erlang-root-lib-path
      (dolist (p0 ehead-user-porject-list root-path)
        (when (and (not root-path) (string-match-p p0 cur-path))
          (setq root-path p0)))
      (if root-path
          root-path
        (ehead-input-project-root-path)))))


(defun ehead-input-project-root-path ()
  "Input the main project root path in minibuffer, and cache in the variable 'ehead-user-porject-list'."
  (let* ((path (read-string "(Add a project root path): " (ehead-search-raw-app-root-path))))
    (if (file-exists-p path)
        (push path ehead-user-porject-list)
      (error "EHEAD WARN: '%s' not exist." path))
    (message "EHEAD LOG: Added a project root path: %s." path)
    path))


(defun ehead-search-raw-app-root-path (&optional cur-path)
  "Find the erlang app root path and return path string.
If not found rebar.config or .git/, return default-directory."
  (unless cur-path
    (setq cur-path default-directory))
  (expand-file-name (or (locate-dominating-file cur-path "rebar.config")
                        (locate-dominating-file cur-path ".git")
                        cur-path)))


(defun ehead-lookup-ehead-user-porject-list ()
  "Show all ehead-user-porject stored."
  (interactive)
  (message "%s" ehead-user-porject-list))



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


(defun ehead-find-include-at-point-id ()
  "Custom -include line identifier."
  (save-excursion
    (move-end-of-line 1)
    (let* ((bound (point)))
      (move-beginning-of-line 1)
      (cond ((re-search-forward ehead-regex-include bound t) ;; match normal include
             (list 'include nil (match-string-no-properties 1) nil))
            ((re-search-forward ehead-regex-include-lib bound t) ;; match lib include
             (list 'include-lib nil (match-string-no-properties 1) nil))
            (t nil)))))

(defun ehead-find-include-at-point-jump (kind hrl)
  "Jump to hrl file which at point."
  (let* ((main-path (ehead-root-path-main-project))
         hrl-path)
    (cond ((eq kind 'include)
           (setq hrl-path (ehead-ensure-project-hrl-file-path hrl)))
          ((eq kind 'include-lib)
           (setq hrl-path (or (ehead-check-standard-hrl-lib hrl)
                              (ehead-check-deps-hrl-lib hrl main-path))))
          (t nil))
    (if hrl-path
        (ehead-find-hrl-at-point-goto hrl-path)
      (message "EHEAD WARN: Not found hrl file %s." hrl)
      nil)))


(defun ehead-ensure-project-hrl-file-path (hrl)
  "Get the abs path of project include file."
  (let* ((app-path (ehead-root-path-sub-app))
         hrl-path)
    (or (file-exists-p (setq hrl-path (expand-file-name (concat "include/" hrl) app-path)))
        (file-exists-p (setq hrl-path (expand-file-name hrl)))
        (setq hrl-path nil))
    hrl-path))


(defun ehead-check-standard-hrl-lib (hrl)
  "Get hrl file which is matched in '-include_lib(*).' abs path."
  (let* (hrl-path hrl-lib hrl-lib-other hrl-lib-path)
    (if (not (string-match "\\(.+?\\)/\\(.+\\)" hrl))
        (ehead-ensure-project-hrl-file-path hrl)
      (and (setq hrl-lib (ignore-errors (match-string 1 hrl)))
           (setq hrl-lib-other (ignore-errors (match-string 2 hrl)))
           (setq hrl-lib-path (car (ehead-shell-find-dir ehead-erlang-root-lib-path hrl-lib)))
           (setq hrl-path (concat hrl-lib-path "/" hrl-lib-other))
           (file-exists-p hrl-path)
           hrl-path)
      )))


(defun ehead-check-deps-hrl-lib (hrl project-path)
  "Get hrl file which is matched in '-include_lib(*).' abs path."
  (let* (hrl-path hrl-name)
    (setq hrl-name (if (string-match ".*/\\(.+?\\)$" hrl)
                       (match-string 1 hrl)
                     nil))
    (if hrl-name
        (let* ((ms (ehead-shell-find-file project-path hrl-name))
               found)
          (while (and (not found) (setq hrl-path (pop ms)))
            (when (string-match hrl hrl-path)
              (setq found t))
            )
          (if found
              hrl-path
            nil))
      nil)))


(defun ehead-shell-find-file (path name &optional modulep)
  "Wrap shell command 'find' to find the file which match NAME. If MODULEP is t, auto append '.erl' to NAME."
  (let* ((str (if modulep (concat name ".erl") name)))
    (split-string (shell-command-to-string (concat "find " path " -type f -name " str " 2>/dev/null")))))


(defun ehead-shell-find-dir (path prefix-dir)
  "Wrap shell command 'find' to find the dir which match PREFIX-DIR."
  (let* ((re (concat prefix-dir "*")))
    (split-string (shell-command-to-string (concat "find " path " -type d -name " re " 2>/dev/null")))))


(defun ehead-find-hrl-at-point-goto (hrl-path)
  "Jump to file and add origin to ring."
  (ehead-add-to-ring)
  (find-file hrl-path))


(defun ehead-jump-to-function-definition (m f a)
  "Jump to definition of function."
  (progn
    (ehead-add-to-ring)
    (cond ((eq m (erlang-get-module))
           (ehead-search-function m f a))
          ((and (eq m nil) (or (member f erlang-int-bifs) (member f erlang-ext-bifs)))
           (ehead-jump-to-module-function-definition "erlang" f a))
          (t
           (ehead-jump-to-module-function-definition m f a)))))


(defun ehead-jump-to-module-function-definition (m f a)
  "Jump to other module definition of function."
  (let* ((main-path (ehead-root-path-main-project))
         erl-path)
    (cond ((setq erl-path (car (ehead-shell-find-file ehead-erlang-root-lib-path m t)))
           (find-file erl-path)
           (ehead-search-function m f a))
          ((setq erl-path (car (ehead-shell-find-file main-path m t)))
           (find-file erl-path)
           (ehead-search-function m f a))
          (t
           (message "EHEAD WARN: Not found %s:%s/%s" m f a)))))


(defun ehead-search-function (m f a)
  "Do search."
  (or (and f
           (ehead-search-function-or-type f a)
           ;; (recenter-top-bottom)
           (ehead-found-flash-region))
      (message "EHEAD WARN: Not found %s:%s/%s" m f a)))


(defun ehead-search-function-or-type (name arity &optional type)
  "Goto the definition of NAME/ARITY in the current buffer.
Value is non-nil if search is successful.
Copy from distel."
  (let ((re (concat "^" (and type "-type\\s-*") (regexp-quote name) "\\s-*("))
        found)
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (let ((case-fold-search nil)) (re-search-forward re nil t)))
        (save-excursion
          (move-beginning-of-line 1)
          (when (or (null arity) (eq (erlang-get-function-arity) arity))
            (setq found (line-beginning-position))))))
    (cond
     (found
      (goto-char found))
     ((and arity (not type))
      (message "EHEAD WARN: Ignore arity %s ..." arity)
      (ehead-search-function-or-type name nil nil))
     ((not type)
      (ehead-search-function-or-type name arity t))
     (t
      nil))))


(defun ehead-found-flash-region ()
  "Temporarily highlight region between BEG and END for TIMEOUT seconds.
BEG and END default to the beginning and end of current line.
Copy from distel."
  (let ((o (make-overlay (line-beginning-position)
                         (line-end-position))))
    (overlay-put o 'face 'match)
    (run-with-timer 0.3 nil #'delete-overlay o)))





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
  (let ((main-path (ehead-root-path-main-project)))
    (grep (concat "cd " main-path " && " "grep --color -nH -re \"" pattern "\" " subdir)))
  )


(defun ehead-get-func-reference ()
  "Find all references of the pointed function from others module in current erlang project."
  (interactive)
  (let* (
         (m (erlang-get-module))
         (s (thing-at-point 'symbol))
         (f (and s (regexp-quote s)))
         )
    (if (and m f)
        (ehead-grep (concat m ":" f) "src/")
      (message "EHEAD WARN: Wrong symbol at point.")))
  )





;; Compile
(defun ehead-compile ()
  "Compile the project by running shell command 'rebar3 compile'."
  (interactive)
  (let* ((main-path (ehead-root-path-main-project))
         (cmd (and main-path (concat "cd " main-path " && " "rebar3 compile"))))
    (if (file-exists-p (expand-file-name "rebar.config" main-path))
        (compilation-start cmd nil (lambda (ignore) "*Ehead-Compile*"))
      (message "EHEAD WARN: Not found erlang project."))))



(provide 'ehead)
