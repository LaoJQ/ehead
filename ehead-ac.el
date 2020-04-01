;;; ahead-ac.el --- simply auto completion of function when type.
;;
;; deps:
;;  erlang
;;  auto-complete


(defun ac-ehead-candidates ()
  "ac source for completing 'Module:Function'"
  (let* ((project-path (or (ehead-project-root-path) "./"))
         module buffer exports erl-path candidates)
    (save-excursion
      (re-search-backward "[^A-Za-z0-9_]\\([A-Za-z0-9_]+\\)\\s-*:" nil t)
      (setq module (match-string-no-properties 1)))
    (when module
      (or (setq erl-path (car (ehead-shell-find ehead-erlang-root-lib-path module nil t)))
          (setq erl-path (car (ehead-shell-find project-path module nil t))))
      (cond ((not erl-path)
             ;; (message "EHEAD DEBUG: module %s" module)
             nil)
            ((setq buffer (find-buffer-visiting erl-path))
             ;; (message "EHEAD DEBUG: module %s" module)
             (with-current-buffer buffer (setq exports (erlang-get-export))))
            ((file-exists-p erl-path)
             ;; (message "EHEAD DEBUG: module %s" module)
             (setq buffer (find-file-noselect erl-path t))
             (with-current-buffer buffer (setq exports (erlang-get-export)))
             (kill-buffer buffer))))
    (dolist (elm exports)
      (push (car elm) candidates))
    candidates
    ))


(ac-define-source ehead
  '((candidates . ac-ehead-candidates)
    (prefix . ":\\([A-Za-z0-9_]*\\)")
    (requires . 0)
    (symbol . "f")
    (cache)
    ))



(provide 'ehead-ac)
