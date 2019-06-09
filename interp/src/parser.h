#ifndef PARSER_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define PARSER_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include "std.h"

namespace Interp_Parser {

    using Interp_Impl::Std_String;

    struct Input_Stream {
        virtual ~Input_Stream () {}
        virtual int peek () = 0;
        virtual int get () = 0;
        virtual bool eof () = 0;
    };

    struct Std_Input_Stream final : Input_Stream {
        std::istream* istream;

        Std_Input_Stream (std::istream* istream_)
        : istream(istream_)
        { }

        int peek () { return istream->peek(); }
        int get () { return istream->get(); }
        bool eof () { return istream->eof(); }
    };

    typedef std::vector<char> Buffer;

    bool contains (Buffer& s, int c);

    struct Input {

        Input_Stream* istream;
        Buffer buffer;
        size_t begin_line;
        size_t begin_column;
        size_t line;
        size_t column;

        Input (Input_Stream* istream_)
        : istream(istream_)
        , begin_line(0)
        , begin_column(0)
        , line(1)
        , column(1)
        { }

        static const int eof_char = std::char_traits<char>::eof();

        int get () {
            int c = istream->get();
            if      (c == '\n')     { ++line; column = 1; }
            else if (c != eof_char) { ++column; }
            return c;
        }

        int peek () { return istream->peek(); }

        void add (int c) { buffer.push_back(c); }

        void clear () {
            buffer.clear();
            begin_line = line;
            begin_column = column;
        }
    };

    struct Token {
        enum class Type : char {
            Invalid,
            Number,
            Identifier,
            String,
            Whitespace,
            Eof,
        };
        Type type;
        size_t line;
        size_t column;
        Buffer string;

        bool is_eof () const {
            return type == Type::Eof;
        }

        Std_String to_std_string () const {
            return Std_String(string.begin(), string.end());
        }
    };

    struct Parser final : Input {

        bool error;

        Parser (Input_Stream* istream)
        : Input(istream)
        , error(false)
        { }

        void raise () {
            error = true;
        }

        Token make_token (Token::Type type) {
            return { type, begin_line, begin_column, buffer };
        }

        Token read_next_token ();

        bool number ();
        bool identifier ();
        bool whitespace ();

        bool ident_head_char ();
        bool ident_tail_char ();
        bool digit ();
        bool space ();

        static int is_space (int c) { return c <= 0x20; }
        static int is_digit (int c) { return c >= 0x30 && c <= 0x39; }
        static int is_ident_head (int c) { return c != eof_char && !is_space(c) && !is_digit(c); }
        static int is_ident_tail (int c) { return c != eof_char && !is_space(c); }

    private:

        bool eof () { return peek() == eof_char; }

        typedef bool (Parser::*Production) ();

        template <int Check(int)> bool
        char1 () {
            int c = peek();
            if (c == eof_char) { return false; }
            if (!Check(c)) { return false; }
            add(c);
            get();
            return true;
        }

        // weakness: must qualify with the "&Parser::" noise

        template <Production Prod> bool
        zero_or_more () {
            while ((this->*Prod)()) { }
            return true;
        }

        template <Production Prod> bool
        one_or_more () {
            int n = 0;
            while ((this->*Prod)()) { ++n; }
            return n > 0;
        }
    };

    // deprecated
    // type-level
    struct Parser2 {

        template <int Check(int)>
        struct char1 { bool operator() (Parser* self) {
            int c = self->peek();
            if (!Check(c)) { return false; }
            self->add(c);
            self->get();
            return true;
        } };

        template <typename Prod>
        struct zero_or_more { bool operator() (Parser* self) {
            Prod prod;
            while (prod(self)) { }
            return true;
        } };

        using alpha = char1<isalpha>;
        using ident = zero_or_more<alpha>;
        // weakness: can't write using A = B || C; must write using A = or<B,C> instead
        // C++ doesn't have type-level operators

        bool doparse (Parser* self) {
            ident parse;
            return parse(self);
        }

    };
}

namespace {
    using Token = Interp_Parser::Token;

    const char* enum_name (Token::Type t);

    std::ostream& operator<< (std::ostream& os, Token& t);
}

#endif
