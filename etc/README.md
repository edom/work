# Configuration files

```
# DIR is the directory containing this README.md file.

DIR=$PWD

# link TARGET LINK_NAME
link() {
    ln -s -f "$@"
}

link $DIR/vimrc ~/.vimrc
link $DIR/vscode_user_settings.json ~/.config/Code/User/settings.json
link $DIR/gdbinit ~/.gdbinit
link $DIR/swiplrc.pro ~/.swiplrc
link $DIR/bashrc ~/.bashrc
link $DIR/sysctl.d/98-sysrq.conf /etc/sysctl.d/98-sysrq.conf
```

More info:
- https://code.visualstudio.com/docs/getstarted/settings
- http://www.swi-prolog.org/pldoc/man?section=initfile
    - Alternative to swiplrc: use swipl -f switch
