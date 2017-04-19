# Cold-Turkey Head-First Dive into Kanji

This document assumes that you are using Ubuntu 14.04.

## How to build this document

### First-time prerequisites

```
sudo apt-get install latex-cjk-common texlive-xetex texlive-fonts-extra fonts-hanazono
```

### Building

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

## Resources

### Sequencing and prioritization

[Primary school kanji list](https://en.wikipedia.org/wiki/Ky%C5%8Diku_kanji)

### Texts

Test your kanji knowledge.

[Asahi news](http://www.asahi.com/news/)

[Mainichi news](http://mainichi.jp/today/)

[Japanese Wikipedia](https://ja.wikipedia.org/wiki/%E3%83%A1%E3%82%A4%E3%83%B3%E3%83%9A%E3%83%BC%E3%82%B8)
or
[random article](https://ja.wikipedia.org/wiki/%E7%89%B9%E5%88%A5:%E3%81%8A%E3%81%BE%E3%81%8B%E3%81%9B%E8%A1%A8%E7%A4%BA)

### Dictionaries and input methods

On Ubuntu 14.04, you can install Anthy on Ibus.
It can be used for Japanese text input
by typing the romaji on US keyboard.

[English Wiktionary](https://en.wiktionary.org/wiki/)
and
[Japanese Wiktionary](https://ja.wiktionary.org/wiki/)
(English and Japanese are the user interface language,
not the source or target language)

[Weblio English-to-Japanese dictionary](http://ejje.weblio.jp/)
uses Japanese WordNet for example sentences.

### Rough machine translation

[Google Translate Japanese-to-English](https://translate.google.com/#ja/en/)
can be used to get a rough translation between two languages.
Do not assume that the translation is correct.

### Grammar

Michiel Kamermans's [An introduction to Japanese: syntax, grammar, and language](https://pomax.github.io/nrGrammar/)

### Others

Wiktionary's [1000 Japanese basic words](https://en.wiktionary.org/wiki/Appendix:1000_Japanese_basic_words)
and [Japanese lemmas](https://en.wiktionary.org/wiki/Category:Japanese_lemmas).

Wikipedia's [List of kanji by concept](https://en.wikipedia.org/wiki/List_of_kanji_by_concept)
