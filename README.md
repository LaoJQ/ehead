# ehead

Ehead is a plugins for Erlang code in emacs. It can help to jump to the definition of record or macro, jump to hrl file when point at include line, grep in erlang project.

## deps

Ehead depend on the standard erlang-mode library, so it need to load erlang-mode at first. For example:

```elisp
(setq erlang-root-dir "/YOUR/ERLANG/INSTALL/PATH/")
(add-to-list 'load-path (car (file-expand-wildcards (expand-file-name "lib/tools-*/emacs" erlang-root-dir))))
(add-to-list 'exec-path (expand-file-name "bin" erlang-root-dir))
(require 'erlang-start)
```

## install

Now, ehead provide some functions to use, `'ehead-jump` and `'ehead-back` for code jump, `'ehead-grep-mark` and `'ehead-grep-input` for grep in project, and you can set the key bind for then. And it need to set the erlang install path to the variable `ehead-erlang-root-path`.

```elisp
(add-to-list 'load-path "/YOUR/EHEAD/PATH/")
(require 'ehead)

(define-key erlang-extended-mode-map "\M-." 'ehead-jump)
(define-key erlang-extended-mode-map "\M-," 'ehead-back)
(define-key erlang-extended-mode-map "\C-cm" 'ehead-grep-mark)
(define-key erlang-extended-mode-map "\C-ci" 'ehead-grep-input)

(setq ehead-erlang-root-path erlang-root-dir")
```

## use with distel

However, ehead just can jump in record or macro, if you want to jump to the definition of function, you may need the other plugin [distel](https://github.com/massemanet/distel). Ehead can simply integrate with distel:

```elisp
;; First, init distel.

(setq ehead-jump-ring erl-find-history-ring)
(setq ehead-jump-other-kind-interface 'erl-find-source-under-point)
```

## TODO

+ Support to jump to the definition of function in ehead, without distel which use a erlang node as a language server.
