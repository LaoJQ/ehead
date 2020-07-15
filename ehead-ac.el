;;; ahead-ac.el --- auto completion of function, record, macro and other which in the same mode buffer.
;;
;; deps:
;;  erlang
;;  auto-complete


(defun ac-ehead-function-candidates ()
  "ac source for completing 'Module:Function'"
  (let* ((main-path (ehead-root-path-main-project))
         module buffer exports erl-path candidates)
    (save-excursion
      (re-search-backward "[^A-Za-z0-9_]\\([A-Za-z0-9_]+\\)\\s-*:" nil t)
      (setq module (match-string-no-properties 1)))
    (when module
      (or (setq erl-path (car (ehead-shell-find-file ehead-erlang-root-lib-path module t)))
          (setq erl-path (car (ehead-shell-find-file main-path module t))))
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
      (push (concat (car elm) "/" (number-to-string (cdr elm))) candidates))
    candidates
    ))


(defun ac-ehead-function-action ()
  "Alter the form of selected candidate which is 'F/A' to 'F'."
  (let* (delta)
    (save-excursion
      (let* ((old-point (point))
             (new-point (re-search-backward "/[0-9]" (line-beginning-position) t)))
        (when new-point
          (setq delta (- new-point old-point)))))
    (when delta
      (delete-char delta))))


(ac-define-source ehead-function
  '((candidates . ac-ehead-function-candidates)
    (action . ac-ehead-function-action)
    (prefix . ":\\([A-Za-z0-9_]*\\)")
    (requires . 0)
    (symbol . "f")
    (cache)
    ))



(defun ehead-ac-cands-collect-all (buffer re searched)
  "Collect string match RE in the whole BUFFER, as well as the hrl file included by BUFFER.
SEARCHED is to avoid endless loop by hrl file circular reference."
  (with-current-buffer buffer
    (let* (cands hrls hrl-buffer ret)
      (push (buffer-file-name buffer) searched)
      (setq cands (append cands (ehead-ac-cands-collect buffer re)))
      (setq hrls (ehead-find-all-include))
      (dolist (hrl hrls)
        (unless (member hrl searched)
            ;; (message "EHEAD WARN: circular reference %s in %s" hrl buffer)
          (setq ret nil)
          (cond ((setq hrl-buffer (find-buffer-visiting hrl))
                 (setq ret (ehead-ac-cands-collect-all hrl-buffer re searched))
                 )
                (t
                 (setq hrl-buffer (find-file-noselect hrl t))
                 (setq ret (ehead-ac-cands-collect-all hrl-buffer re searched))
                 (kill-buffer hrl-buffer)
                 )
                )
          (setq cands (append cands (car ret)))
          (setq searched (nth 1 ret))
          )
        )
      (list cands searched))))


(defun ehead-ac-cands-collect (buffer re)
  "Collect string match RE in the whole BUFFER."
  (let* (found cands)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (setq found (ignore-errors (re-search-forward re) (match-string-no-properties 1)))
        (push found cands)))
    cands))


(defun ac-ehead-record-candidates ()
  "ac source for completing '#Record'"
  (car (ehead-ac-cands-collect-all (current-buffer) "-record(\\s-*\\(.+?\\)\\s-*," nil)))


(ac-define-source ehead-record
  '((candidates . ac-ehead-record-candidates)
    (prefix . "#\\([A-Za-z0-9_]*\\)")
    (requires . 0)
    (symbol . "r")
    (cache)
    ))


(defun ac-ehead-macro-candidates ()
  "ac source for completing '?MACRO'"
  (car (ehead-ac-cands-collect-all (current-buffer) "-define(\\s-*\\(.+?\\)\\s-*[,(]" nil)))


(ac-define-source ehead-macro
  '((candidates . ac-ehead-macro-candidates)
    (prefix . "?\\([A-Za-z0-9_]*\\)")
    (requires . 0)
    (symbol . "c")
    (cache)
    ))



(defconst ehead-mornal-ac-source-prefix "[^A-Za-z0-9_:#?]\\([A-Za-z0-9_]*\\)")


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
                         ac-source-ehead-function
                         ac-source-ehead-record
                         ac-source-ehead-macro
                         )
  "Ehead's all ac-sources.")



(provide 'ehead-ac)
