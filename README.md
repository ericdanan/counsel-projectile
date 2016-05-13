[![MELPA](https://melpa.org/packages/counsel-projectile-badge.svg)](https://melpa.org/#/counsel-projectile)

# Description

This library tries to do something similar to `helm-projectile`, but using `ivy` instead of `helm`. The main function is `counsel-projectile`.

# Installation

Install the package from `MELPA` or `el-get`, or clone this repository somewhere in your load path.

# Usage

The main function is `counsel-projectile`. It starts an `ivy-read` session with the list of `projectile` known projects as candidates. The current project, if any, comes first in this list. The default action switches to the selected project (therefore calling `projectile-switch-project-action`). There are several additional actions to switch project with a specific `projectile-switch-project-action` (eg `projectile-vc`). The action list can be customized since `ivy-read` is called with `:caller 'counsel-projectile`.

In the action list, a new function `counsel-projectile-find-file` is used instead of the standard `projectile-find-file`. It simply proposes an additional action to find the file in another window (thus bringing together `projectile-find-file` and `projectile-find-file-other-window`). You may also call this function directly or bind it to some key. Similarly for two other new functions: `counsel-projectile-find-dir` and `counsel-projectile-switch-to-buffer`.

# Contributors

Many thanks to @abo-abo and @DamienCassou who encouraged and helped me to start this repository.