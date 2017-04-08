# Bootstrap your Japanese with English

This document assumes that you are using Ubuntu 14.04.

## How to build this document

```
sudo apt-get install texlive-xetex texlive-fonts-extra fonts-takao
```

```
./make
```

## Character frequency analysis

See the [source code](analyze) for documentation.

```
./analyze
```

## Kanji lookup

This requires the UTF-8 dictionary files
that come prepackaged with the gjiten package.

```
sudo apt-get install gjiten
```

See the usage notes in the [source code](lookup) of `lookup`.

See also [gjiten original readme](https://github.com/2ion/gjiten/blob/master/README.original).

### Using lookup with Vim

(a.k.a. gjiten vim duct-tape "integration")

Make sure your CapsLock is off.

In Vim:

```
:source script.vim
```

See the [source code](script.vim).

Press F3 or F4 to look-up the kanji at the cursor.
If you press F4, only verbal phrases are shown.

Press j for the next line.

Press Space for the next page.

Press G (that is shift-g) to go to the bottom of the result
and see the kanji description.

Press q (or Enter when vim says so) to quit the result.
If you accidentally begin recording, press Q again.
