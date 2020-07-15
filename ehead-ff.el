;;; ahead-ff.el --- fuzzy find in erlang project and standard lib.
;;
;; deps:
;;  erlang
;;  find-file-in-project


(defvar ehead-fuzzy-find-module-std-cache nil
  "Cache standard erlang module collection for fuzzy file.")

(defun ehead-fuzzy-find-module-std-cache-set (&optional clean)
  "Set or clean ehead-fuzzy-find-module-std-cache."
  (setq ehead-fuzzy-find-module-std-cache
        (if clean nil
          (ehead-fuzzy-find-all-module-collection ehead-erlang-root-lib-path))))

(defun ehead-fuzzy-find-module-std-cache-clean ()
  "Clean ehead-fuzzy-find-module-std-cache"
  (interactive)
  (ehead-fuzzy-find-module-std-cache-set t))

(defun ehead-fuzzy-find-module-std-cache-get (&optional clean)
  "Read ehead-fuzzy-find-module-std-cache. If nil, do set."
  (or ehead-fuzzy-find-module-std-cache (ehead-fuzzy-find-module-std-cache-set)))


(defun ehead-fuzzy-find-module ()
  "Fuzzy find project module file in project or standard module file."
  (interactive)
  (let* ((main-path (ehead-root-path-main-project))
         (app-collection (ehead-fuzzy-find-all-module-collection main-path))
         (collection (append app-collection (ehead-fuzzy-find-module-std-cache-get))))
    (if (not collection)
        (message "EHEAD WARN: Not found erl file in project.")
      (ffip-completing-read
       (format "Module in project %s: " main-path)
       collection
       `(lambda (cand) (find-file (cdr cand)))
       )
      )
    )
  )

(defun ehead-fuzzy-find-all-module-collection (path)
  "Get module collection in special PATH."
  (when path
    (mapcar `(lambda (file)
               (string-match ".*/\\([a-zA-Z0-9_]+\\).erl" file)
               (cons (match-string-no-properties 1 file) file)
               )
            (split-string (shell-command-to-string (concat "find " path " -type f -not -ipath \"*/_build/test/*\" -name \"*.erl\" 2>/dev/null"))))
    ))



(provide 'ehead-ff)
