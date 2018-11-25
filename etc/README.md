# Configuration files

```
# DIR is the directory containing this README.md file.

DIR=$PWD

# link TARGET LINK_NAME
link() {
    ln -s -f "$@"
}

link $DIR/vimrc ~/.vimrc

# https://code.visualstudio.com/docs/getstarted/settings

link $DIR/vscode_user_settings.json ~/.config/Code/User/settings.json

link $DIR/gdbinit ~/.gdbinit

# http://www.swi-prolog.org/pldoc/man?section=initfile
link $DIR/swiplrc.pro ~/.swiplrc
# or use -f switch
```
