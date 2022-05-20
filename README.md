# dtache.el

<a href="http://elpa.gnu.org/packages/dtache.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/packages/dtache.svg"/></a>
<a href="http://elpa.gnu.org/devel/dtache.html"><img alt="GNU-devel ELPA" src="https://elpa.gnu.org/devel/dtache.svg"/></a>
<a href="https://melpa.org/#/dtache"><img alt="MELPA" src="https://melpa.org/packages/dtache-badge.svg"/></a>
<a href="https://stable.melpa.org/#/dtache"><img alt="MELPA Stable" src="https://stable.melpa.org/packages/dtache-badge.svg"/></a>
<a href="https://builds.sr.ht/~niklaseklund/dtache.el/commits/main/.build.yml"><img alt="Build" src="https://builds.sr.ht/~niklaseklund/dtache.el/commits/main/.build.yml.svg"/></a>

# Introduction

`dtache` is a package to launch, and manage, detached processes. The idea is that these processes are detached from the Emacs process and the package can make Emacs seamlessly attach to these processes. This enables users to launch processes that can survive when Emacs itself is being shutdown. The package relies on the program [dtach](https://github.com/crigler/dtach), in order to make this functionality possible.

Internally the package transforms the dtache process into a `dtache-session` object, which essentially is a text-based representation of the process. All `dtache-session` objects are stored in a lisp file, and the output of all sessions are captured into individual logs.

The package supports integration with multiple packages, here is a list of the built-in packages that are supported:

- `shell`
- `eshell`
- `compile`
- `org`
- `dired`

# Features

Since a `dtache-session` contain all the output of the process as well as data such as, `what command` was run, `which directory` the process was launched etc, it opens up the possibility for the following features:

- `Unlimited scrollback:` All the output from a `dtache-session` is always accessible
- `Remote support:` Full support for running on remote hosts. See `Remote support` section of the README
- `Notifications:` The package will monitor all dtache sessions and notify when a session has finished
- `Post compilation`: If the package has been configured to use `dtache-env` it will know the exit status of a session. That enables the package to post compile the output of a session to enable Emacs's built-in functionality of navigating between errors in the output.
- `Annotations:` When selecting a session all are presented with a rich set of annotations
- `Actions:` The package provides actions to act on a session:
  + `kill` a an active session
  + `rerun` a session
  + `copy` the output of a session
  + `diff` the output of two different sessions

# Installation

The package is available on [GNU ELPA](https://elpa.gnu.org) and [MELPA](https://melpa.org/), and for users of the [GNU Guix package manager](https://guix.gnu.org/) there is a guix package.

# Configuration

The prerequisite for `dtache.el` is that the user has the program `dtach` installed.

## Use-package example

A minimal `use-package` configuration.

``` emacs-lisp
(use-package dtache
  :init
  (dtache-init)
  :bind (;; Replace `async-shell-command' with `dtache-shell-command'
         ([remap async-shell-command] . dtache-shell-command)
         ;; Replace `compile' with `dtache-compile'
         ([remap compile] . dtache-compile)
         ([remap recompile] . dtache-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap dtache-open-session] . dtache-consult-session))
  :custom ((dtache-env "<path_to_dtache-env>")
           (dtache-show-output-on-attach t)
           (dtache-shell-history-file "~/.bash_history")))
```

The users are required to call `dtache-init`. This function orchestrates the integration with all other internal and external packages that `dtache.el` supports. These are:

- `compile`
- `dired`
- `dired-rsync`
- `embark`
- `eshell`
- `org`
- `projectile`
- `shell`
- `vterm`

All of the integration are configured to enable lazy-loading. Meaning that if you are not a user of `projectile` that code that handles the integration will never load.

However if you do want to disable the integration with a specific package, or enable for a subset of the packages use the variables `dtache-init-allow-list` or `dtache-init-block-list`.

# Usage 

The idea is that users can choose to either:
- `create`: a dtache session and attach to it
- `detach`: from a dtache session
- `attach`: to a dtache session

In the integration of `dtache.el` with other packages these commands are always existent, with the exception for `org-mode`.

To detach from a `dtache-session` in any of the modes, use the universal `dtache-detach-session` command. The keybinding for this command is defined by the `dtache-detach-key` variable, which by default has the value `C-c C-d`.

## General

To interact with a session the package provides the command `detached-open-session`. This provides a convenient completion interface, enriched with annotations to provide useful information about the sessions. The `detached-open-session` command is implemented as a do what I mean command. This results in `dtache` performing different actions depending on the state of a session. The actions can be configured based on the `origin` of the session. The user can have one set of configurations for sessions started in `shell` which is different from those started in `compile`.

The actions are controlled by the customizable variables named `dtache-.*-session-action`. They come preconfigured but if you don't like the behavior of `dtache-open-session` these variables allows for tweaking the experience.

- If the session is `active`, call the sessions `attach` function
- If the session is `inactive` call the sessions `view` function, which by default performs a post-compile on the session if its status is `failure` otherwise the sessions raw output is opened.
    
The package also provides additional commands to interact with a session.

| Command (Keybinding)                | Description                                 |
|-------------------------------------|---------------------------------------------|
| dtache-view-session (v)           | View a session's output                     |
| dtache-attach-session (a)         | Attach to a session                         |
| dtache-tail-session  (t)          | Tail the output of an active session        |
| dtache-diff-session (=)           | Diff a session with another session         |
| dtache-compile-session (c)        | Open the session output in compilation mode |
| dtache-rerun-session (r)          | Rerun a session                             |
| dtache-insert-session-command (i) | Insert the session's command at point       |
| dtache-copy-session-command (w)   | Copy the session's shell command            |
| dtache-copy-session (W)           | Copy the session's output                   |
| dtache-kill-session (k)           | Kill an active session                      |
| dtache-delete-session (d)         | Delete an inactive session                  |

These commands are available through the `dtache-action-map`. The user can bind the action map to a keybinding of choice. For example

``` emacs-lisp
(global-set-key (kbd "C-c d") dtache-action-map)
```

Then upon invocation the user can choose an action, keybindings listed in the table above, and then choose a session to perform the action upon. For those using `embark` this will not be necessary as `dtache-init` sets up integration with embark actions.

## Shell command

The `dtache-shell-command` is for the Emacs users that are accustomed to running shell commands from `M-x shell-command` or `M-x async-shell-command`.

## Shell

A minor mode named `dtache-shell-mode` is provided, and will be enabled in `shell`. The commands that are implemented are:

| Command                       | Description                    | Keybinding          |
|-------------------------------|--------------------------------|---------------------|
| dtache-shell-send-input     | Run command with dtache      | <S-return>          |
| dtache-shell-attach-session | Attach to a dtache session   | <C-return>          |
| dtache-detach-session       | Detach from a dtache session | dtache-detach-key |

## Eshell

A minor mode named `dtache-eshell-mode` is provided, and will be enabled in `eshell`. The commands that are implemented are:

| Command                        | Description                    | Keybinding          |
|--------------------------------|--------------------------------|---------------------|
| dtache-eshell-send-input     | Run command with dtache      | <S-return>          |
| dtache-eshell-attach-session | Attach to a dtache session   | <C-return>          |
| dtache-detach-session        | Detach from a dtache session | dtache-detach-key |

## Org babel

The package implements an additional header argument for `ob-shell`. The header argument is `:detached t`. When provided it will enable the code inside a src block to be run with `dtache`. Since org is not providing any live updates on the output the session is created with `detached-sesion-mode` set to `create`. This means that if you want to access the output of the session you do that the same way you would for any other type of session. The `detached-org` works both with and without the `:session` header argument.

```
#+begin_src sh :dtache t
    cd ~/code
    ls -la
#+end_src

#+RESULTS:
: [dtache]
```

## Compile

The package implements the commands `detached-compile` and `detached-compile-recompile`, which are thin wrappers around the original `compile` and `recompile` commands. The users should be able to use the former as replacements for the latter without noticing any difference except from the possibility to `detach`.

## Consult

The command `detached-consult-session` is a replacement for `detached-open-session` using the [consult](https://github.com/minad/consult) package. The difference is that the consult command provides multiple session sources, which is defined in the `detached-consult-sources` variable. Users can customize which sources to use, as well as use individual sources in other `consult` commands, such as `consult-buffer`. The users can also narrow the list of sessions by entering a key. The list of supported keys are:

| Type                  | Key |
|-----------------------+-----|
| Active sessions       | a   |
| Inactive sessions     | i   |
| Successful sessions   | s   |
| Failed sessions       | f   |
| Local host sessions   | l   |
| Remote host sessions  | r   |
| Current host sessions | c   |

# Customization

## Customizable variables

The package provides the following customizable variables.

| Name                                 | Description                                                                      |
|--------------------------------------|----------------------------------------------------------------------------------|
| dtache-session-directory           | A host specific directory to store sessions in                                   |
| dtache-db-directory                | A localhost specific directory to store the database                             |
| dtache-dtach-program               | Name or path to the `dtach` program                                              |
| dtache-shell-program               | Name or path to the `shell` that `dtache.el` should use                        |
| dtache-timer-configuration         | Configuration of the timer that runs on remote hosts                             |
| dtache-env                         | Name or path to the `dtache-env` script                                        |
| dtache-annotation-format           | A list of annotations that should be present in completion                       |
| dtache-command-format              | A configuration for displaying a session command                                 |
| dtache-tail-interval               | How often `dtache.el` should refresh the output when tailing                   |
| dtache-nonattachable-commands      | A list of commands that should be considered nonattachable                       |
| dtache-notification-function       | Specifies which function to issue notifications with                             |
| dtache-detach-key                  | Specifies which keybinding to use to detach from a session                       |
| dtache-shell-command-initial-input | Enables latest value in history to be used as initial input                      |
| dtache-filter-ansi-sequences       | Specifies if `dtache.el` will use ansi-color to filter out escape sequences    |
| dtache-show-output-command         | Specifies if `dtache.el` should show the session's output when attaching to it |

Apart from those variables there is also the different `action` variables, which can be configured differently depending on the origin of the session.

| Name                                  | Description                                                     |
|---------------------------------------|-----------------------------------------------------------------|
| dtache-shell-command-session-action | Actions for sessions launched with `dtache-shell-command`     |
| dtache-eshell-session-action        | Actions for sessions launched with `dtache-eshell-send-input` |
| dtache-shell-session-action         | Actions for sessions launched with `dtache-shell-send-input`  |
| dtache-compile-session-action       | Actions for sessions launched with `dtache-compile`           |
| dtache-org-session-action           | Actions for sessions launched with `dtache-org`               |

## Remote support

The `dtache` supports [Connection Local Variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/Connection-Local-Variables.html) which allows the user to customize the variables used by `dtache` when running on a remote host. This example shows how the following variables are customized for all remote hosts.

``` emacs-lisp
(connection-local-set-profile-variables
 'remote-dtache
 '((dtache-env . "~/bin/dtache-env")
   (dtache-shell-program . "/bin/bash")
   (dtache-shell-history-file . "~/.bash_history")
   (dtache-session-directory . "~/tmp")
   (dtache-dtach-program . "/home/user/.local/bin/dtach")))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh") 'remote-dtache)
```

## Completion annotations

Users can customize the appearance of annotations in `dtache-open-session` by modifying the `dtache-annotation-format`. The default annotation format is the following.

``` emacs-lisp
(defvar dtache-annotation-format
  `((:width 3 :function dtache--state-str :face dtache-state-face)
    (:width 3 :function dtache--status-str :face dtache-failure-face)
    (:width 10 :function dtache--host-str :face dtache-host-face)
    (:width 40 :function dtache--working-dir-str :face dtache-working-dir-face)
    (:width 30 :function dtache--metadata-str :face dtache-metadata-face)
    (:width 10 :function dtache--duration-str :face dtache-duration-face)
    (:width 8 :function dtache--size-str :face dtache-size-face)
    (:width 12 :function dtache--creation-str :face dtache-creation-face))
  "The format of the annotations.")
```

## Status deduction

Users are encouraged to define the `detached-env` variable. It should point to the `detached-env` script, which is provided in the repository. This script allows sessions to communicate the status of a session when it transitions to inactive. When configured properly `dtache` will be able to set the status of a session to either `success` or `failure`.

``` emacs-lisp
(setq dtache-env "/path/to/repo/dtache-env")
```

## Metadata annotators

The user can configure any number of annotators to run upon creation of a session. Here is an example of an annotator which captures the git branch name, if the session is started in a git repository.

``` emacs-lisp
(defun my/dtache--session-git-branch ()
  "Return current git branch."
  (let ((git-directory (locate-dominating-file "." ".git")))
    (when git-directory
      (let ((args '("name-rev" "--name-only" "HEAD")))
        (with-temp-buffer
          (apply #'process-file `("git" nil t nil ,@args))
          (string-trim (buffer-string)))))))
```

Next add the annotation function to the `dtache-metadata-annotators-alist` together with a symbol describing the property.

``` emacs-lisp
(setq dtache-metadata-annotators-alist '((branch . my/dtache--session-git-branch))
```

## Nonattachable commands

To be able to both attach to a dtach session as well as logging its output `dtache` relies on the usage of `tee`. However it is possible that the user tries to run a command which involves a program that doesn't integrate well with tee. In those situations the output could be delayed until the session ends, which is not preferable.

For these situations `dtache.el` provides the `dtache-nonattachable-commands` variable. This is a list of regular expressions. Any command that matches any of the strings will be getting the property `attachable` set to false.
``` emacs-lisp
(setq dtache-nonattachable-commands '("^ls"))
```

Here a command beginning with `ls` would from now on be considered nonattachable.

## Colors in sessions

The package needs to use a trick to get programs programs such as `git` or `grep` to show color in their outputs. This is because these commands only use colors and ansi sequences if they are being run in a terminal, as opposed to a pipe. The `detached-env` therefore has two different modes. The mode can be either `plain-text` or `terminal-data`, the latter is now the default. The `detached-env` program then uses the `script` command to make programs run in `dtache` think they are inside of a full-featured terminal, and therefore can log their raw terminal data.

The drawback is that there can be commands which generates escape sequences that the package supports and will therefore mess up the output for some commands. If you detect such an incompatible command you can add a regexp that matches that command to the list `detached-env-plain-text-commands`. By doing so `dtache` will instruct `detached-env` to run those commands in plain-text mode.

# Tips & Tricks

The `dtache.el` package integrates with core Emacs packages as well as 3rd party packages. Integration is orchestrated in the `dtache-init.el`. In this section you can find tips for integrations that are not supported in the package itself.

## Alert

By default `dtache` uses the built in `notifications` library to issue a notification. This solution uses `dbus` but if that doesn't work for the user there is the possibility to set the `detached-notification-function` to `detached-state-transition-echo-message` to use the echo area instead. If that doesn't suffice there is the possibility to use the [alert](https://github.com/jwiegley/alert) package to get a system notification instead.

``` emacs-lisp
(defun my/state-dtache-transition-alert-notification (session)
  "Send an `alert' notification when SESSION becomes inactive."
  (let ((status (car (dtache--session-status session)))
        (host (car (dtache--session-host session))))
    (alert (dtache--session-command session)
           :title (pcase status
                    ('success (format "Dtache finished [%s]" host))
                    ('failure (format "Dtache failed [%s]" host)))
           :severity (pcase status
                       ('success 'moderate)
                       ('failure 'high)))))

(setq dtache-notification-function #'my/dtache-state-transition-alert-notification)
```

# Versions

Information about larger changes that has been made between versions can be found in the `CHANGELOG.org`

# Support

The `dtache.el` package should work on `Linux` and `macOS`. It is regularly tested on `Ubuntu` and `GNU Guix System`.

# Contributions

The package is part of [ELPA](https://elpa.gnu.org/) which means that if you want to contribute you must have a [copyright assignment](https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html).

# Acknowledgments

This package wouldn't have been were it is today without these contributors.

## Code contributors

- [rosetail](https://gitlab.com/rosetail)
- [protesilaos](https://lists.sr.ht/~protesilaos)

## Idea contributors

- [rosetail](https://gitlab.com/rosetail) for all the great ideas and improvements to the package. Without those contributions `dtache.el` would be a less sophisticated package.
- [Troy de Freitas](https://gitlab.com/ntdef) for solving the problem of getting `dtache.el` to work with `filenotify` on macOS.
- [Daniel Mendler](https://gitlab.com/minad) for helping out in improving `dtache.el`, among other things integration with other packages such as `embark` and `consult`.
- [Ambrevar](https://gitlab.com/ambrevar) who indirectly contributed by inspiring me with his [yes eshell is my main shell](https://www.reddit.com/r/emacs/comments/6y3q4k/yes_eshell_is_my_main_shell/). It was through that I discovered his [package-eshell-detach](https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/package-eshell-detach.el) which got me into the idea of using `dtach` as a base for detached shell commands.

