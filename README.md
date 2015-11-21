# gitconfig.el

Emacs lisp interface to work with git-config variables

## Installation

### ELPA

gitconfig.el is available on both community maintained repositories -
[Marmalade](http://marmalade-repo.org/) and
[MELPA](https://melpa.org/). Just run `M-x package-install
[RET] gitconfig [RET]`
inside your emacs and you're ready to go.

If you're not already using ELPA, check the [emacswiki](http://www.emacswiki.org/emacs/ELPA) page to get
familiar with it.

### Manual

```lisp
(add-to-list 'load-path "~/path/to/gitconfig.el/")
(require 'gitconfig)
```

## Usage

### Interactive functions

<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>gitconfig-execute-command</code></td>
        <td>Run <code>git config</code> with custom ARGUMENTS and display it in
            the <code>gitconfig-buffer-name</code></td>
    </tr>
</table>

### Non-interactive functions

<table>
    <tr>
        <th>Function</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>(gitconfig-current-inside-git-repository-p)</code></td>
        <td>Return <code>t</code> if <code>default-directory</code> is a git repository</td>
    </tr>
    <tr>
        <td><code>(gitconfig-path-to-git-repository)</code></td>
        <td>Return the absolute path of the current git repository</td>
    </tr>
    <tr>
        <td><code>(gitconfig-get-variables LOCATION)</code></td>
        <td>Get all variables for the given LOCATION and return it as a hash table</td>
    </tr>
    <tr>
        <td><code>(gitconfig-set-variable LOCATION NAME VALUE)</code></td>
        <td>Set a specific LOCATION variable with a given NAME and VALUE</td>
    </tr>
    <tr>
        <td><code>(gitconfig-get-variable LOCATION NAME)</code></td>
        <td>Return a specific LOCATION variable for the given NAME</td>
    </tr>
    <tr>
        <td><code>(gitconfig-delete-variable LOCATION NAME)</code></td>
        <td>Delete a specific LOCATION variable for the given NAME</td>
    </tr>
    <tr>
        <td><code>(gitconfig-get-local-variables)</code></td>
        <td>Return all <code>git config --local --list</code> variables as hash table</td>
    </tr>
    <tr>
        <td><code>(gitconfig-get-global-variables)</code></td>
        <td>Return all <code>git config --global --list</code> variables as hash table</td>
    </tr>
    <tr>
        <td><code>(gitconfig-get-system-variables)</code></td>
        <td>Return all <code>git config --system --list</code> variables as hash table</td>
    </tr>
    <tr>
        <td><code>(gitconfig-get-local-variable NAME)</code></td>
        <td>Return a specific <code>git config --local --list</code> variable by the given NAME</td>
    </tr>
    <tr>
        <td><code>(gitconfig-get-global-variable NAME)</code></td>
        <td>Return a specific <code>git config --global --list</code> variable by the given NAME</td>
    </tr>
    <tr>
        <td><code>(gitconfig-get-system-variable NAME)</code></td>
        <td>Return a specific <code>git config --system --list</code> variable by the given NAME</td>
    </tr>
</table>

## Configuration

`gitconfig-git-command` Path to the executable `git` shell command.

```lisp
(setq gitconfig-git-command "/usr/local/bin/git")
```

`gitconfig-buffer-name` Name of the buffer used for git shell output.

```lisp
(setq gitconfig-buffer-name "*GITCONFIG*")
```

## Contributions are very welcome!

1. Fork gitconfig.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!
