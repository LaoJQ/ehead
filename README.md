# ehead

Ehead is a plugins for Erlang code in emacs. It can help to jump to the definition of record or macro.

## deps

Ehead depend on the standard erlang-mode library, so it need to load erlang-mode at first. For example:

```elisp
(setq erlang-root-dir "/YOUR/ERLANG/INSTALL/PATH/")
(add-to-list 'load-path (car (file-expand-wildcards (expand-file-name "lib/tools-*/emacs" erlang-root-dir))))
(add-to-list 'exec-path (expand-file-name "bin" erlang-root-dir))
(require 'erlang-start)
```

## install

Now, ehead provide two function to use, *jump to* and *jump back*, and you can set the key bind for then.

```elisp
(add-to-list 'load-path "/YOUR/EHEAD/PATH/")
(require 'ehead)

(define-key erlang-extended-mode-map "\M-." 'ehead-jump)
(define-key erlang-extended-mode-map "\M-," 'ehead-back)
```

## use with distel

However, ehead just work for record and macro, if you want to jump to the definition of function, you may need the other plugin [distel](https://github.com/massemanet/distel). It can simply integrate with distel:

```elisp
;; First, init distel.

(setq ehead-jump-ring erl-find-history-ring)
(setq ehead-jump-other-kind-interface 'erl-find-source-under-point)
```

## TODO

+ Support to jump to header file when points in "^-include*".
+ Support to search include_lib file.
