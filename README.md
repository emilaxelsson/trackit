# trackit

`trackit` is a command-line tool that listens for changes in a user-supplied directory. Whenever there is a change, a custom command is executed and its standard output is shown live in the terminal.

![](images/trackit.gif)

## Examples

Show a live listing of the files in the current directory:

    > trackit --watch-dir=. --command="ls --color"

Show a live revision graph of a Git repository:

    > GIT_DIR=`git rev-parse --git-dir`
    > trackit --watch-tree=$GIT_DIR --command="git log --graph --all --oneline --decorate --color"

## Installation

`trackit` can be installed from [Hackage](https://hackage.haskell.org/package/trackit) using Cabal:

    > cabal install trackit

## Usage

Run `trackit -h` to get more information about available flags.

`trackit` starts a new buffer in the terminal and uses it to display the output of the command given as the `--command` flag. The display reacts to the following keyboard events:

  * **q** - Quit `trackit`.
  * **arrow keys** - Scroll the output buffer (also with PgUp/PgDown/Home/End).
  * **space key** - Re-run the command and update the buffer. This is useful if no watch directory is provided, or if the output is affected by events outside of the watch directory.

When multiple changes occur in a short time in the watched directory (e.g. when switching branches in a repository), it may not be desired to have `trackit` react to every single change. This is especially the case if the monitored command is expensive (e.g. `git log` in a large repository). For this reason, `trackit` requires a *stabilization period* before running an update. If an event occurs during that period, the period clock is restarted and the update is delayed further.

The stabilization period can be set in milliseconds using the `--stabilization` flag. A lower value gives quicker response times, but increases the risk of getting spurious updates when a tight sequence changes occurs in the watched directory. The default stabilization period is 200 ms.

## Comparison to `watch`

`trackit` offers two main advantages over the similar tool [watch](https://linux.die.net/man/1/watch):

  1. `trackit` only reacts to file system changes. This avoids having to run a potentially costly command periodically. For example, the following command can easily consume a substantial part of your processor's cycles when run in a large repository:

     ```
     > watch -c -t -n 0,5 -- git log --graph --all --oneline --decorate --color
     ```

  2. `trackit` supports scrolling, and keeps the scrolled view even if the output is updated.
