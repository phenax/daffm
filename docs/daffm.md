# NAME

daffm - dumb as fuck file manager

# SYNOPSIS

**daffm** \[**-c** *config-file*\] \[**directory_or_file**\]

# DESCRIPTION

**daffm** is a dumb file manager/directory explorer that is meant to
integrate with system utils for file management features.

# OPTIONS

**-c, \--config config-path**

:   Load toml config from file. If path is prefixed with @, will use
    alternate config. Example: -c \@foo will load
    \$XDG_CONFIG_HOME/daffm/config.foo.toml Default:
    \$XDG_CONFIG_HOME/daffm/config.toml

**-v**

:   Prints version information.

**-h, \--help**

:   Prints help information.

# CONFIGURATION

**keymap**

:   A mapping of key sequences to the commands. Example: gdl = \"cd
    \~/Downloads\"

<!-- -->

**extend**

:   Which config file to extend. This can use file paths or @-prefix for
    alternate configs Example: extend = \"@custom-config\"

<!-- -->

**opener**

:   Shell script that is executed when open command is invoked on a
    file. Example: extend = \"xdg-open %\" \# Opens file under cursor
    (ignores selections)

# COMMANDS

**back**

:   Goes back one level in directory.

<!-- -->

**cd \<dir\>**

:   Change current directory shown.

<!-- -->

**cmdline-enter**

:   Switch focus to the command line input

<!-- -->

**cmdline-leave**

:   Exit cmdline if focused

<!-- -->

**cmdline-set \<text \...\>**

:   Enter the command line input with given text prefilled

<!-- -->

**eval \<command\> \[\...args\]**

:   Runs command in shell. Any line in stdout that begins with \<daffm\>
    is evaluated as a daffm command. This can be used to create more
    dynamic commands and keybinds.

<!-- -->

**map \<key-sequence\> \<command\>**

:   Add a new keymap to run given command when the key sequence is
    pressed

<!-- -->

**move \<pos\>**

:   Move the cursor to position. To move the cursor relative to current
    position, use +/- (eg: \'move +1\' goes down 5 lines). You can also
    use \'move \$\' to move the cursor to the last item in the list.

<!-- -->

**open**

:   If cursor is on a directory, cd into the directory, otherwise, run
    opener

<!-- -->

**quit**

:   Quit

<!-- -->

**reload**

:   Reloads the current directory and shows the latest information.
    Daffm doesn\'t watch the directory for changes but after a call to a
    shell or eval command, the directory contents are reloaded.

<!-- -->

**search \[text\]**

:   Search the given text in the filenames in current directory. If text
    is empty, stops search highlighting

<!-- -->

**search-next**

:   Jump to next item in the search results

<!-- -->

**search-prev**

:   Jump to previous item in the search results

<!-- -->

**selection-clear**

:   Clear all selections

<!-- -->

**selection-toggle**

:   Toggle selection for file under cursor

<!-- -->

**shell \<command\> \[\...args\]**

:   Run a command in shell. If a command exits with non-zero status
    code, a \"Press any key to continue\" prompt is shown.

<!-- -->

**shell! \<command\> \[\...args\]**

:   Same as shell but it adds a \"Press any key to continue\" prompt
    after the command runs regardless of exit status code.

# COMMAND SUBSTITUIONS

The following pattern (%,%d,%f,%s,%F,%S) are replaced with absolute
paths

%: File under cursor

%d: Current directory

%s: Selected files separated by spaces

%S: Selected files separated by newlines

%f: Same as %s but if there are no selections, uses file under cursor

%F: Same as %S but if there are no selections, uses file under cursor

# AUTHORS

Akshay Nair \<phenax5@gmail.com\>

# LICENSE

See the LICENSE file for the terms of redistribution.

# BUGS AND FEATURE REQUESTS

https://github.com/phenax/daffm/issues
