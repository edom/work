/*
C++ parser idea 1:
1 method ~ 1 non-terminal
Correlate method and nonterminal
Correlate method calling sequence and nonterminal sequence

C++ parser idea 2: parser combinator
1 C++ class ~ 1 Haskell Parsec combinator ~ 1 non-terminal
*/

#include <cctype>
#include <cstdint>
#include <functional>

class Parser {
    public:
        void get_number (char* s, size_t n, size_t begin, size_t* end) {
            size_t i = begin;
            while (i < n && isdigit(s[i])) {
                ++i;
            }
            *end = i;
        }

        void get_ident (char* s, size_t n, size_t begin, size_t* end) {
            size_t i = begin;
            while (i < n && isalnum(s[i])) {
                ++i;
            }
            *end = i;
        }
};
