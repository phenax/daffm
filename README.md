# Daffm
Dumb as fuck file manager is a minimal tui file manager

![screenshot](./media/screenshot.jpg)

## Install
- Clone the repo and build it: `cabal build daffm` or `nix build`
- Nix flakes users can also install it as a flake: `github:phenax/daffm#daffm`


## Config
Configuration is managed in toml.
By default it will try to load `$XDG_CONFIG_HOME/daffm/config.toml`.
You can load config in a different path using `daffm -c <path-to-config>`.
You can also store alternate configs in `$XDG_CONFIG_HOME/daffm/config.custom-thing.toml` and load it as `daffm -c @custom-thing`.

Heres an example config for reference:

```toml
# `opener` runs when opening a file or selections
opener = """
echo "%F" | while IFS= read file; do
  case "$(file --mime-type "$file" -bL)" in
    text/*) $EDITOR "$file" ;;
    *) xdg-open "$file" >/dev/null 2>&1 ;;
  esac
done;
"""

[keymap]
gdl = "cd ~/Downloads"
gdc = "cd ~/Documents"
gp = "cd ~/Pictures"

rn = "!!echo '%F' | vidir -v -" # Uses vidir (moreutils) to rename selected files/directories
md = "cmdline-set !mkdir -p " # Prefills cmdline
mf = "cmdline-set !touch "
dd = "!rm -rfi %f"
sdd = "!sudo rm -rfi %f"
cp = "cmdline-set !cp -f % %"

# Copes file to clipboard
"<space>yy" = """!
xclip -selection clipboard -t $(file --mime-type '%' -bL) -i '%'
"""
```

The substituions (%,%f,%s,%F,%S) are replaced with absolute file paths
- `%`: File under cursor
- `%s`: Selected files separated by spaces
- `%S`: Selected files separated by newlines
- `%f`: Same as `%s` but if there are no selections, uses file under cursor
- `%F`: Same as `%S` but if there are no selections, uses file under cursor


## Default keys (no need to define these in config)

```toml
[keymap]
q = "quit"
rr = "reload"
"!" = "cmdline-set !"
":" = "cmdline-enter"
l = "open"
h = "back"
"<cr>" = "open"
"<bs>" = "back"
v = "selection-toggle" # select/unselect files
"<tab>" = "selection-toggle"
C = "selection-clear"
"~" = "cd ~"
gh = "cd ~" # Go home
"$" = "$SHELL" # drop to shell
gx = "!xdg-open % >/dev/null 2>&1" # Open externally
gcfg = "cd ~/.config/daffm" # Open default configurations directory
```


## Commands

WIP

