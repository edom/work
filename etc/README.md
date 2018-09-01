# Configuration files

```
# DIR is the directory containing this README.md file.

DIR=$PWD

# link TARGET LINK_NAME
link() {
    ln -s -f "$@"
}

link $DIR/.vimrc ~/.vimrc

# https://code.visualstudio.com/docs/getstarted/settings

link $DIR/vscode_user_settings.json ~/.config/Code/User/settings.json
```
