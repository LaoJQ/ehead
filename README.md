# ehead

Ehead is a plugins for Erlang code program in emacs with below features:

+ Jump to definition of function.
+ Jump to record or macro.
+ Jump to include file.
+ Grep code in project.
+ Needn't extra erlang node for code navigation.

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

(add-hook 'erlang-mode-hook
          (lambda ()
            (define-key erlang-mode-map "\M-." 'ehead-jump)
            (define-key erlang-mode-map "\M-," 'ehead-back)
            (define-key erlang-mode-map "\C-cm" 'ehead-grep-mark)
            (define-key erlang-mode-map "\C-ci" 'ehead-grep-input)
            ))

(setq ehead-erlang-root-path erlang-root-dir")
```

## TODO

+ Support jump to the import function.
