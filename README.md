# ehead

Ehead is a plugins for Erlang code program in emacs with below features:

+ Jump to definition of function.
+ Jump to record or macro.
+ Jump to include file.
+ Grep code in project.
+ Auto completion of function when type simply.
+ Compile project by reabr3.
+ Fuzzy find module in project and standard lib.
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

Now, ehead provide some functions to use, `'ehead-jump` and `'ehead-back` for code jump, `'ehead-grep-mark` and `'ehead-grep-input` for grep in project, `'ehead-compile` for compile project, and you can set the key bind for then. And it need to set the erlang install path to the variable `ehead-erlang-root-path`.

It also simply support to auto complete of function when type `Module:`, which basic on [auto-complete](https://github.com/auto-complete/auto-complete). So, if you want it, please install `auto-complete` first.

It also support to fuzzy find module file in project or standard lib, use `'ehead-fuzzy-find-module` function, which depends on [find-file-in-project](https://github.com/technomancy/find-file-in-project).

```elisp
(add-to-list 'load-path "/YOUR/EHEAD/PATH/")
(require 'ehead)
(require 'ehead-ac)
(require 'ehead-ff)

(add-hook 'erlang-mode-hook
          (lambda ()
            ;; bind key
            (define-key erlang-mode-map "\M-." 'ehead-jump)
            (define-key erlang-mode-map "\M-," 'ehead-back)
            (define-key erlang-mode-map "\C-cm" 'ehead-grep-mark)
            (define-key erlang-mode-map "\C-ci" 'ehead-grep-input)
            (define-key erlang-mode-map "\C-c'" 'ehead-compile)
            ;; set ac-sources if you want
            (setq ac-sources ehead-ac-source)
            ;; bind key for 'ehead-fuzzy-find-module if you want
            (define-key erlang-mode-map "\C-c\C-p'" 'ehead-fuzzy-find-module)
            ))

(setq ehead-erlang-root-lib-path (expand-file-name "lib" erlang-root-dir))
```

## TODO
