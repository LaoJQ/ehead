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


(defconst ehead-mornal-ac-source-prefix "[^A-Za-z0-9_:]\\([A-Za-z0-9_]*\\)")


;; Similar to ac-source-words-in-same-mode-buffers
(ac-define-source ehead-same-mode-buffers
  `((init . ac-update-word-index)
    (prefix . ,ehead-mornal-ac-source-prefix)
    (candidates . (ac-word-candidates
                   (lambda (buffer)
                     (derived-mode-p (buffer-local-value 'major-mode buffer)))))))

;; Similar to ac-source-dictionary
(ac-define-source ehead-dictionary
  `((candidates . ac-buffer-dictionary)
    (prefix . ,ehead-mornal-ac-source-prefix)
    (symbol . "d")))



(defvar ehead-ac-source (list
                         ac-source-ehead-same-mode-buffers
                         ac-source-ehead-dictionary
                         ac-source-ehead
                         )
  "Ehead's all ac-sources.")



(provide 'ehead-ac)
