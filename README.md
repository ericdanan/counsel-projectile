[![MELPA](https://melpa.org/packages/counsel-projectile-badge.svg)](https://melpa.org/#/counsel-projectile)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Description](#description)
- [News](#news)
- [Installation](#installation)
- [Usage](#usage)
    - [Getting started](#getting-started)
    - [Summary of interactive commands](#summary-of-interactive-commands)
    - [The `counsel-projectile` command](#the-counsel-projectile-command)
    - [The `counsel-projectile-switch-project` command](#the-counsel-projectile-switch-project-command)
    - [The `counsel-projectile-find-file` command](#the-counsel-projectile-find-file-command)
    - [The `counsel-projectile-find-dir` command](#the-counsel-projectile-find-dir-command)
    - [The `counsel-projectile-switch-to-buffer` command](#the-counsel-projectile-switch-to-buffer-command)
    - [The `counsel-projectile-grep` command](#the-counsel-projectile-grep-command)
    - [The `counsel-projectile-ag` command](#the-counsel-projectile-ag-command)
    - [The `counsel-projectile-rg` command](#the-counsel-projectile-rg-command)
    - [The `counsel-projectile-org-capture` command](#the-counsel-projectile-org-capture-command)
- [Configuration](#configuration)
    - [Enabling counsel-projectile mode when emacs starts](#enabling-counsel-projectile-mode-when-emacs-starts)
    - [Customizing action lists](#customizing-action-lists)
    - [Setting `counsel-projectile-org-capture` templates](#setting-counsel-projectile-org-capture-templates)
    - [Removing the current project or buffer from the list of candidates](#removing-the-current-project-or-buffer-from-the-list-of-candidates)
    - [Initial input for the project search commands](#initial-input-for-the-project-search-commands)
- [Upgrading from previous version](#upgrading-from-previous-version)
    - [Key bindings](#key-bindings)
    - [Action lists](#action-lists)
    - [Minibuffer keymap](#minibuffer-keymap)
- [Contributors](#contributors)

<!-- markdown-toc end -->

# Description
[Projectile](https://github.com/bbatsov/projectile) has native support for using [ivy](https://github.com/abo-abo/swiper) as its completion system. Counsel-projectile provides further ivy integration into projectile by taking advantage of ivy's support for selecting from a list of actions and applying an action without leaving the completion session. Concretely, counsel-projectile defines replacements for existing projectile commands as well as new commands that have no projectile counterparts. A minor mode is also provided that adds key bindings for all these commands on top of the projectile key bindings.
# News
- [2017-12-18] New version `0.2`. If you are upgrading from `0.1`, please read [here](#upgrading-from-previous-version) about important changes.
- [2016-04-12] First version `0.1`.
# Installation
Install the package from [MELPA](https://melpa.org) or [el-get](https://github.com/dimitri/el-get), or clone this repository somewhere in your load path.
# Usage
## Getting started
To turn on counsel-projectile mode, either call the command `counsel-projectile-mode` or use the Customize interface to toggle on the variable `counsel-projectile-mode`. This will turn on projectile mode, thus enabling all projectile key bindings, and add the counsel-projectile key bindings on top of them.

The counsel-projectile key bindings either remap existing projectile commands to their counsel-projectile replacements (e.g. <kbd>C-c p f</kbd> now calls `counsel-projectile-find-file` instead of `projectile-find-file`) or bind keys to counsel-projectile commands that have no projectile counterparts (e.g. <kbd>C-c p SPC</kbd> calls the command `counsel-projectile`).

Calling the command `counsel-projectile-mode` once again or toggling off the variable `counsel-projectile-mode` disables the counsel-projectile key bindings and turns off projectile mode.

Note that if you turn on projectile mode but not counsel-projectile mode, the counsel-projectile commands can still be called with <kbd>M-x</kbd>, only the key bindings for these commands are not enabled.
## Summary of interactive commands
Replacements for existing commands:

| Key binding          | Command                               | Description                 |
| :------------------- | :------------------------------------ | :-------------------------- |
| <kbd>C-c p p</kbd>   | `counsel-projectile-switch-project`   | Switch project              |
| <kbd>C-c p f</kbd>   | `counsel-projectile-find-file`        | Jump to a project file      |
| <kbd>C-c p d</kbd>   | `counsel-projectile-find-dir`         | Jump to a project directory |
| <kbd>C-c p b</kbd>   | `counsel-projectile-switch-to-buffer` | Jump to a project buffer    |
| <kbd>C-c p s g</kbd> | `counsel-projectile-grep`             | Search project with grep    |
| <kbd>C-c p s s</kbd> | `counsel-projectile-ag`               | Search project with ag      |

New commands:

| Key binding          | Command                          | Description                                         |
| :------------------- | :------------------------------- | :-------------------------------------------------- |
| <kbd>C-c p SPC</kbd> | `counsel-projectile`             | Jump to a project buffer or file, or switch project |
| <kbd>C-c p s r</kbd> | `counsel-projectile-rg`          | Search project with rg                              |
| <kbd>C-c p O</kbd>   | `counsel-projectile-org-capture` | Org-capture into project                            |
## The `counsel-projectile` command
Default key binding: <kbd>C-c p SPC</kbd>.

This command lets you quickly jump to a project buffer or file. It uses ivy to display in the minibuffer a list of all project buffers as well as all project files that are not currently visited by a buffer. Buffers are fontified according to their major mode and files are fontified as virtual buffers, as in the command `ivy-switch-buffer`. As in all ivy commands, you can use <kbd>M-o</kbd> / <kbd>C-M-o</kbd> + <kbd>key</kbd> to select from a list of actions to apply (or <kbd>M-RET</kbd> / <kbd>C-M-RET</kbd> to apply the default action) to the selected candidate:

| Key          | Action                                                                             |
| :----------- | :--------------------------------------------------------------------------------- |
| <kbd>o</kbd> | Open buffer or file in current window (default action)                             |
| <kbd>j</kbd> | Open buffer or file in other window                                                |
| <kbd>x</kbd> | Open file externally (does nothing for buffers)                                    |
| <kbd>r</kbd> | Open file as root (does nothing for buffers)                                       |
| <kbd>m</kbd> | Find file manually: call `counsel-find-file` from buffer or file's directory |
| <kbd>p</kbd> | Switch project: call `counsel-projectile-switch-project` (see below)               |

If not called inside a project, `counsel-projectile` first offers to select a project to switch to by calling `counsel-projectile-switch-project` (see below). Once you select a project and hit <kbd>RET</kbd>, it lets you jump to a buffer or file in this project as described above.
## The `counsel-projectile-switch-project` command
Default key binding: <kbd>C-c p p</kbd>.

This command is a replacement for `projectile-switch-project`. It adds the possibility to select from a list of switch-project actions to apply to the selected project:

| Key           | Action                                                                                  |
| :------------ | :-------------------------------------------------------------------------------------- |
| <kbd>o</kbd>  | Jump to a project buffer or file: call `counsel-projectile` (default action; see above) |
| <kbd>f</kbd>  | Jump to a project file: call `counsel-projectile-find-file` (see below)                 |
| <kbd>d</kbd>  | Jump to a project directory: call `counsel-projectile-find-dir` (see below)             |
| <kbd>b</kbd>  | Jump to a project buffer: call `counsel-projectile-switch-to-buffer` (see below)        |
| <kbd>m</kbd>  | Find file manually: call `counsel-find-file` from the project root                      |
| <kbd>S</kbd>  | Save all project buffers                                                                |
| <kbd>k</kbd>  | Kill all project buffers                                                                |
| <kbd>K</kbd>  | Remove project from the list of known projects                                          |
| <kbd>c</kbd>  | Run project compilation command                                                         |
| <kbd>C</kbd>  | Run project configure command                                                           |
| <kbd>E</kbd>  | Edit project directory-local variables                                                  |
| <kbd>v</kbd>  | Open project in vc-dir / magit / monky                                                  |
| <kbd>sg</kbd> | Search project with grep: call `counsel-projectile-grep` (see below)                    |
| <kbd>ss</kbd> | Search project with ag: call `counsel-projectile-ag` (see below)                        |
| <kbd>sr</kbd> | Search project with rg: call `counsel-projectile-rg` (see below)                        |
| <kbd>xs</kbd> | Invoke shell from the project root                                                      |
| <kbd>xe</kbd> | Invoke eshell from the project root                                                     |
| <kbd>xt</kbd> | Invoke term from the project root                                                       |
| <kbd>O</kbd>  | Org-capture into project: call `counsel-projectile-org-capture` (see below)             |
## The `counsel-projectile-find-file` command
Default key binding: <kbd>C-c p f</kbd>.

This command is a replacement for `projectile-find-file`. It displays a list of all project files and offers several actions:

| Key          | Action                                                                   |
| :----------- | :----------------------------------------------------------------------- |
| <kbd>o</kbd> | Open file in current window (default action)                             |
| <kbd>j</kbd> | Open file in other window                                                |
| <kbd>x</kbd> | Open file externally (does nothing for buffers)                          |
| <kbd>r</kbd> | Open file as root (does nothing for buffers)                             |
| <kbd>m</kbd> | Find file manually: call `counsel-find-file` from file's directory |
| <kbd>p</kbd> | Switch project: call `counsel-projectile-switch-project` (see above)     |
## The `counsel-projectile-find-dir` command
Default key binding: <kbd>C-c p d</kbd>.

This command is a replacement for `projectile-find-dir`. It displays a list of all project directories and offers several actions:

| Key          | Action                                                               |
| :----------- | :------------------------------------------------------------------- |
| <kbd>o</kbd> | Open directory with `dired` in current window (default action)       |
| <kbd>j</kbd> | Open director with `dired` in other window                           |
| <kbd>m</kbd> | Find file manually: call `counsel-find-file` from directory          |
| <kbd>p</kbd> | Switch project: call `counsel-projectile-switch-project` (see above) |
## The `counsel-projectile-switch-to-buffer` command
Default key binding: <kbd>C-c p b</kbd>.

This command is a replacement for `projectile-switch-to-buffer`. It displays a list of all project buffers and offers several actions:

| Key          | Action                                                                     |
| :----------- | :------------------------------------------------------------------------- |
| <kbd>o</kbd> | Open buffer in current window (default action)                             |
| <kbd>j</kbd> | Open buffer in other window                                                |
| <kbd>m</kbd> | Find file manually: call `counsel-find-file` from buffer's directory |
| <kbd>p</kbd> | Switch project: call `counsel-projectile-switch-project` (see above)       |
## The `counsel-projectile-grep` command
Default key binding: <kbd>C-c p s g</kbd>.

This command is a replacement for `projectile-grep`. It searches all project files with `grep`, taking advantage of ivy's support for updating the list of candidates after each input (dynamic collections). Each canidate corresponds to a matching line in some project file, and there is only one action that opens that file at that line.
## The `counsel-projectile-ag` command
Default key binding: <kbd>C-c p s s</kbd>.

This command is a replacement for `projectile-ag`. It is similar to `counsel-projectile-grep` (see above) but uses `ag` (the silver searcher) instead of `grep`.
## The `counsel-projectile-rg` command
Default key binding: <kbd>C-c p s r</kbd>.

This command is similar to `counsel-projectile-grep` (see above) but uses `rg` (ripgrep) instead of `grep`.
## The `counsel-projectile-org-capture` command
Default key binding: <kbd>C-c p O</kbd>.

This command lets you capture something (a note, todo item, ...) into the current project using org-mode's `org-capture` (actually `counsel-org-capture`) command. Like `org-capture`, it first lets you select a capture template then file the newly captured information. By default, there is a single template storing the captured information into file `notes.org` in the project root directory, under headline `Tasks`.
# Configuration
## Enabling counsel-projectile mode when emacs starts
To automatically enable counsel-projectile mode when emacs starts, you can either use the Customize interface to toggle on the variable `counsel-projectile-mode` and save your customization, or add `(counsel-projectile-mode)` to your init file.
## Customizing action lists
The lists of available actions (including the default action) for most of the commands above are stored in custom variables. If you set one of these variables, either directly or through the through the Customize interface, the new value will be picked up the next time you invoke the corresponding commmand.

The variable holding the action list for `<command>` is named `<command>-action`. The following action list variables are defined:
- `counsel-projectile-action`
- `counsel-projectile-switch-project-action`
- `counsel-projectile-find-file-action`
- `counsel-projectile-find-dir-action`
- `counsel-projectile-switch-to-buffer-action`

For instance, the default value of `counsel-projectile-action` is:

```emacs-lisp
'(1
  ("o" counsel-projectile-action
   "current window")
  ("j" counsel-projectile-action-other-window
   "other window")
  ("x" counsel-projectile-action-file-extern
   "open file externally")
  ("r" counsel-projectile-action-file-root
   "open file as root")
  ("m" counsel-projectile-action-find-file-manually
   "find file manually")
  ("p" (lambda (_) (counsel-projectile-switch-project))
   "switch project"))
```

The first element is the index of the default action, and the remainig ones are the available actions (a key, an action function, and a name for each action). Thus the default action in this list is the first one (<kbd>o</kbd> key).

Extra actions can be added to these lists or, alternatively, can be set through ivy's `ivy-set-actions` mechanism. If you prefer setting all actions (except the default one) through this mechanism, you can set the action list variable to a single action (e.g. `counsel-projectile-action`) instead of a list. If you are not using the Customize interface and want to amend the value of one of these lists rather than setting it from scratch, you can use the function `counsel-projectile-modify-action`, which lets you easily:
- add, remove, or move an action,
- change an action key, function, or name,
- change the index of the default action.
See its docstring for details.

Although ivy does not support this natively, it is in fact possible to include actions with a two-character key in the list.  To do so, however, it is necessary to also include an action whose key is the first of these two characters and whose action function is `counsel-projectile-prefix-action`. For instance, the default value of `counsel-projectile-switch-project-action` includes the following actions:

```emacs-lisp
  ("s" counsel-projectile-prefix-action
   "search project with grep / ag / rg...")
  ("sg" counsel-projectile-switch-project-action-grep
   "search project with grep")
  ("ss" counsel-projectile-switch-project-action-ag
   "search project with ag")
  ("sr" counsel-projectile-switch-project-action-rg
   "search project with rg")
```
## Setting `counsel-projectile-org-capture` templates
The available capture templates for `counsel-projectile-org-capture` are read from the variable `counsel-projectile-org-capture-templates`. This variable has the same format as the variable `org-capture-templates`, except that in all strings of in an entry’s target slot, all instances of `${root}` and `${name}` are replaced with the current project root and name, respectively.

The default value contains a single template, whose target is:

```emacs-lisp
(file+headline "${root}/notes.org}" "Tasks")
```

This points to headline `Tasks` in file `notes.org` in the project root directory (one file per project).

Another example of a valid target is:

```emacs-lisp
(file+olp "~/notes.org" "${root}" "Tasks")
```

This points to outline path `<project-root>/Tasks` in file `~/notes.org` (same file for all projects).

Templates contexts are read from the variable `counsel-projectile-org-capture-templates-contexts`, which has the same format as `org-capture-templates-contexts`
## Removing the current project or buffer from the list of candidates
By default, when calling `counsel-projectile-switch-project`, the current project (if any) is included in the candidates list and preselected. Similarly, when calling `counsel-projectile-switch-to-buffer`, the current buffer is included in the candidates list and preselected. If you prefer removing these elements from the candidate lists of these commands, you can set the variables `counsel-projectile-remove-current-project` and `counsel-projectile-remove-current-buffer` accordingly.
## Initial input for the project search commands
If you want some initial input to be inserted in the minibuffer every time you call `counsel-projectile-grep`, `counsel-projectile-ag`, or `counsel-projectile-rg`, you can customize the variables `counsel-projectile-grep-initial-input`, `counsel-projectile-ag-initial-input`, or `counsel-projectile-rg-initial-input` accordingly. Each of these variable, if non `nil`, should hold a Lisp expression whose evaluation yields the initial input string. If you use the Customize interface, some choices are proposed based on various versions of the `thing-at-point` function. Note that you can always insert the value of `(ivy-thing-at-point)` by hitting <kbd>M-n</kbd> in the minibuffer.
# Upgrading from previous version
If you are upgrading from version `0.1` to version `0.2`, please read below about important changes, some of which may require you to update your configuration.
## Key bindings
The commands `counsel-projectile-on`, `counsel-projectile-off` and `counsel-projectile-toggle` no longer exist. They are replaced with the counsel-projectile minor mode. You can toggle this mode either by calling the `counsel-projectile-mode` command. or by setting the `counsel-projectile-mode` variable throught the Customize interface.

See [Getting started](#getting-started) above for details.
## Action lists
The available actions for the various counsel-projectile commands are now customized differently:
- The custom variable corresponding to `<command>` is now named `<command-action>` instead of `<command-actions>`.
- This variable now stores all the available actions, including the default action, not only the extra actions.
- It also stores the index of the default action (it is a list whose first element is this index and whose remaining elements are the availabe actions).
- This variable is now used as the value of the `:action` parameter for the command's `ivy-read` call. Hence if you set it outside the Customize interface, you no longer need to call `ivy-set-actions` afterwards. If you set extra actions through `ivy-set-actions`, they will not replace the variable's actions but will be added to them.

See [Customizing action lists](#customizing-action-lists) above for details.

Also, in the default action lists, the keys set for some actions have changed, mainly for the `counsel-projectile-switch-project` command. Indeed, as new actions were added to this command, the corresponding list of keys was becoming somewhat inconsistent. The new keys replicate the default projectile key bindings (for instance, the action to save all project buffers is now called with the key <kbd>S</kbd>, mimicking the default key binding <kbd>C-c p S</kbd> for the command `projectile-save-project-buffers`). When an action calls a command that has no default projectile key binding, its key is chosen among those that are not bound by projectile by default.
## Minibuffer keymap
The minibuffer keymap `counsel-projectile-map` no longer exists. It was only used to bind a key (<kbd>M-SPC</kbd> by default) to the command `counsel-projectile-drop-to-switch-project` exiting the current command and calling `counsel-projectile-switch-project`. The same functionality is now implemented in a simpler way through an action that calls `counsel-projectile-switch-project`, whose key is <kbd>p</kbd> by default. Concretely, you should now hit <kbd>M-o p</kbd> instead of <kbd>M-SP</kbd>.
# Contributors
Counsel-projectile is inspired by [helm-projectile](https://github.com/bbatsov/helm-projectile). Many thanks to [abo-abo](https://github.com/abo-abo) and [DamienCassou](https://github.com/DamienCassou) who encouraged and helped me to start this repository, as well as all contributors and users who have submitted issues and pull requests.
