#!/usr/bin/python3

# Usage:
# 1. You must have saved the page https://en.wikipedia.org/wiki/J%C5%8Dy%C5%8D_kanji into the local HTML file wikipedia-jouyou.html.
# 2. Run python3 jouyou.py <kanji> where <kanji> is UTF-8 encoded

from pyquery import PyQuery

import sys

def main ():

    kanji = sys.argv[1]

    d = PyQuery(filename='wikipedia-jouyou.html')

    (
        d('table.sortable.wikitable tr')
        .filter(lambda i, row: kanji in PyQuery(row).text())
        .each(lambda i, elem: print(PyQuery(elem).text()))
    )

main()
