#include "pch.h"
#include "library.h"
#include "library_machine.cpp"

namespace Machine {
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

    Integer* Value::be_integer_or_throw () {
        throw_if_error();
        Integer* i = be_integer_or_null();
        if (i == nullptr) {
            throw Errors::type_error;
        }
        return i;
    }

    void Value::mutate (Machine& m) {
        m.push(this);
    }

    Type* Integer::type () const { return &Types::Integer; }

    Type* String::type () const { return &Types::String; }

    Type* Error::type () const { return &Types::Error; }

    Key::Key (const char* bytes_, size_t length_) {
        hash = compute_hash(bytes_, length_);
        bytes = new char[length_];
        length = length_;
        memcpy(bytes, bytes_, length_);
    }

    size_t Key::compute_hash (const char* bytes, size_t length) {
        size_t h = 0;
        for (size_t i = 0; i < length; ++i) {
            size_t e = (unsigned char) bytes[i];
            h = 13*h + e;
        }
        return h;
    }

    int Key::compare (const Key& that) const {
        if (this->length < that.length) { return -1; }
        if (this->length > that.length) { return 1; }
        return memcmp(this->bytes, that.bytes, length);
    }

    bool Key::operator== (const Key& that) const { return this->compare(that) == 0; }
    bool Key::operator!= (const Key& that) const { return this->compare(that) != 0; }
    bool Key::operator<= (const Key& that) const { return this->compare(that) <= 0; }
    bool Key::operator< (const Key& that) const { return this->compare(that) < 0; }

    Value* Bindings::lookup (const Key& key) const {
        auto i = bindings.find(key);
        if (i == bindings.end()) {
            return &Errors::key_not_found;
        }
        return i->second;
    }

    Ring_::Ring_ (N stride, N capacity)
    : stride_(stride)
    , capacity_(capacity)
    {
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
}

namespace Parser {
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
    using Token = Parser::Token;

    const char* enum_name (Token::Type t) {
        using E = Token::Type;
        switch (t) {
            case E::Invalid: return "Invalid";
            case E::Number: return "Number";
            case E::Identifier: return "Identifier";
            case E::String: return "String";
            case E::Whitespace: return "Whitespace";
            case E::Eof: return "Eof";
            default: abort();
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
