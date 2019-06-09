#include "pch.h"

#include "library.h"
#include "std.h"

#include "library_machine.cpp"
#include "gc.cpp"
#include "prolog/prolog.cpp"
#include "test.cpp"

namespace Interp_Impl {

    namespace Types {
        Type Integer {"Integer"};
        Type String {"String"};
        Type Operation {"Operation"};
        Type Error {"Error"};
    }

    namespace Errors {
        Error type_error {"type error"};
        Error key_not_found {"key not found"};
        Error stack_underflow {"stack underflow"};
    }

    Integer*
    Value::be_integer_or_throw () {
        throw_if_error();
        Integer* i = be_integer_or_null();
        if (i == nullptr) {
            throw Errors::type_error;
        }
        return i;
    }

    void
    Value::mutate (Machine& m) {
        m.push(this);
    }

    Type* Integer::type () const { return &Types::Integer; }

    Type* String::type () const { return &Types::String; }

    Type* Error::type () const { return &Types::Error; }

    Key::Key (const char* asciz)
    : Key ((byte*)asciz, strlen(asciz))
    { }

    Key::Key (const Key& that)
    : hash(that.hash)
    , length(that.length)
    {
        bytes = new byte[length];
        memcpy(bytes, that.bytes, length);
    }

    static size_t compute_hash (const Key::byte* bytes, size_t length);

    Key::Key (const byte* bytes_, size_t length_) {
        hash = compute_hash(bytes_, length_);
        bytes = new byte[length_];
        length = length_;
        memcpy(bytes, bytes_, length_);
    }

        size_t compute_hash (const Key::byte* bytes, size_t length) {
            size_t h = 0;
            for (size_t i = 0; i < length; ++i) {
                size_t e = bytes[i]; // zero-extend
                h = 13*h + e;
            }
            return h;
        }

    Key::Key (Key&& that) noexcept
    : hash(that.hash)
    , length(that.length)
    , bytes(that.bytes) {
        that.bytes = nullptr;
    }

    Key::~Key () {
        if (bytes != nullptr) {
            delete[] bytes;
        }
    }

    int
    Key::compare (const Key& that) const {
        if (this->length < that.length) { return -1; }
        if (this->length > that.length) { return 1; }
        return memcmp(this->bytes, that.bytes, length);
    }

    bool Key::operator== (const Key& that) const { return this->compare(that) == 0; }
    bool Key::operator!= (const Key& that) const { return this->compare(that) != 0; }
    bool Key::operator<= (const Key& that) const { return this->compare(that) <= 0; }
    bool Key::operator< (const Key& that) const { return this->compare(that) < 0; }

    Std_String
    Key::to_std_string () const {
        static_assert(sizeof(byte) == sizeof(char), "sizeof(byte) == sizeof(char)");
        return Std_String((char*)bytes, length);
    }

    size_t
    Key_Hasher::operator() (const Key& that) const noexcept {
        return that.hash;
    }

    Ring_::Ring_ (N stride, N capacity)
    : stride_(stride)
    , capacity_(capacity)
    {
        static_assert(sizeof(char) == 1, "sizeof(char) == 1");
        static_assert(CHAR_BIT == 8, "CHAR_BIT == 8");
        if (stride <= 0) { abort(); }
        if (capacity <= 0) { abort(); }
        N bytes = capacity * stride;
        if (bytes < capacity) { abort(); }
        if (bytes < stride) { abort(); }
        buffer = new char[bytes];
    }

    Ring_::~Ring_ () {
        if (buffer != nullptr) {
            delete[] buffer;
            buffer = nullptr;
        }
    }

    // -------------------- enhanced C functions

    char*
    safe_strncpy (char* dst, const char* src, size_t n) {
        if (n <= 0) { abort(); }
        size_t src_len = strnlen(src, n);
        size_t limit = std::min(n-1, src_len);
        memcpy(dst, src, limit);
        dst[limit] = 0;
        return dst;
    }

    void
    safe_strftime (char* buffer, size_t buffer_size, const char* format, const tm* time) {
        if (buffer_size <= 0) { abort(); }
        if (strftime(buffer, buffer_size, format, time) == 0) {
            if (*format != 0) {
                // According to man 3 strftime, buffer may be undefined here.
                safe_strncpy(buffer, "(strftime buffer too small)", buffer_size);
            }
        }
    }

    void
    to_local_time (const time_t& in, tm& out) {
        localtime_r(&in, &out);
    }

    Std_String
    format_time (const char* format, const tm& time) {
        char buffer [128]; // This is assumed to be enough.
        safe_strftime(buffer, sizeof(buffer), format, &time);
        return Std_String(buffer);
    }

    void
    get_monotonic_clock (timespec& t) {
        if (clock_gettime(CLOCK_MONOTONIC, &t) != 0) {
            // Which ones of our targeted systems do not have a monotonic clock in 2019?
            perror("get_monotonic_clock");
            abort();
        }
    }

}

namespace Interp_Parser {

    bool Parser::number () { return one_or_more<&Parser::digit>(); }
    bool Parser::identifier () { return ident_head_char() && zero_or_more<&Parser::ident_tail_char>(); }
    bool Parser::whitespace () { return one_or_more<&Parser::space>(); }

    bool Parser::ident_head_char () { return char1<is_ident_head>(); }
    bool Parser::ident_tail_char () { return char1<is_ident_tail>(); }
    bool Parser::digit () { return char1<is_digit>(); }
    bool Parser::space () { return char1<is_space>(); }

    Token Parser::read_next_token () {
        using Type = Token::Type;
        clear();
        Token t;
        if      (number())      { t = make_token(Type::Number); }
        else if (identifier())  { t = make_token(Type::Identifier); }
        else if (whitespace())  { t = make_token(Type::Whitespace); }
        else if (eof())         { t = make_token(Type::Eof); }
        else {
            int c = get();
            add(c);
            t = make_token(Token::Type::Invalid);
        }
        return t;
    }

    bool contains (Buffer& s, int c) {
        return std::find(s.begin(), s.end(), c) != s.end();
    }

}

namespace {

    using Token = Interp_Parser::Token;

    const char* enum_name (Token::Type t) {
        using E = Token::Type;
        switch (t) {
            case E::Invalid: return "Invalid";
            case E::Number: return "Number";
            case E::Identifier: return "Identifier";
            case E::String: return "String";
            case E::Whitespace: return "Whitespace";
            case E::Eof: return "Eof";
            default: abort(); return "Invalid";
        }
    }

    std::ostream& operator<< (std::ostream& os, Token& t) {
        os
        << enum_name(t.type)
        << " " << t.line << ":" << t.column << " ";
        for (auto c : t.string) {
            os << c;
        }
        return os;
    }

}
