#ifndef STC_PL0_HPP_INCLUDED
#define STC_PL0_HPP_INCLUDED

#include <cstdio>
#include "rts.hpp"

namespace stc_pl0 {

using namespace stc_rts;

class Input_Stream {
    public:
        using Position = long;
        virtual ~Input_Stream() { }
        virtual int         read_byte() { return -1; }
        virtual Position    get_position() const { return -1; }
        virtual int         set_position(Position) { return -1; }
        virtual bool        is_valid() const { return false; }
};

class C_Stream : public Input_Stream {
    private:
        std::FILE* _file;
        C_Stream(std::FILE* file) : _file(file) { }
    public:
        C_Stream(const char* path, const char* mode) : _file(std::fopen(path, mode)) { }
        ~C_Stream() override {
            if (nullptr != _file) {
                if (std::fclose(_file) != 0) {
                    //  What should we do if close(2) fails?
                    std::perror("fclose");
                }
                _file = nullptr;
            }
        }
        int read_byte() override {
            int c = std::fgetc(_file);
            return (c == EOF) ? -1 : c;
        }
        Position get_position() const { return ftell(_file); }
        int set_position(Position pos) { return fseek(_file, pos, SEEK_SET); }
        bool is_valid() const { return nullptr != _file; }
};

struct Location {
    using Offset = Input_Stream::Position;
    const char* path;
    Offset      offset;
    Count       line;
    Count       column;
};

struct Token {
    Object_Id   bytes;
    Location    loc;
};

//  We assume:
//
//      -   The input stream is seekable.
//      -   The input stream is not modified while we are parsing.

template <typename T, int n>
class Stack {
    private:
        T       _elems[n];
        int     _top1;
    public:
        Stack() : _top1(0) { }
        ~Stack() { }
        Size limit() const { return n; }
        void push(const T& x) {
            if (_top1 >= n) { panic("stack overflow"); }
            _elems[_top1++] = x;
        }
        T& pop() {
            if (_top1 <= 0) { panic("stack underflow"); }
            return _elems[--_top1];
        }
        T& peek() const {
            if (_top1 <= 0) { panic("stack underflow"); }
            return _elems[_top1 - 1];
        }
};

class Parser {
    private:
        using Offset = Location::Offset;
        World&              _world;
        Input_Stream&       _stream;
        Location            _loc;
        Stack<Location, 8>  _locs;
    public:
        Parser(World& world, Input_Stream& stream)
        : _world(world)
        , _stream(stream)
        , _loc({"", 0, 0, 0})
        { }
    private:
        void update_location(int c) {
            if (c >= 0) {
                switch(c) {
                    case '\n':
                        _loc.column = 0;
                        _loc.line++;
                        break;
                    default:
                        _loc.column++;
                }
                _loc.offset++;
            }
        }

        //  A C++ statement can be thought of as a state transformer.
        //  Should we indicate the failure in a field, and return the char?
        //  Do I want 1 method ~ 1 non-terminal/production?
        //  Is it too much overhead?

        int read_byte() {
            int c = _stream.read_byte();
            update_location(c);
            return c;
        }
        void backtrack_to(const Location& loc) {
            //  What should we do if set_position fails?
            _stream.set_position(loc.offset);
        }
        Token read_token() {
            //  TODO
            panic("not implemented");
        }
};

}

#endif
