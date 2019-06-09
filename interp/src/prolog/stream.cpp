#include <cstdio>
#include "array.cpp"

class Input_stream {
    private:
        FILE* handle;
        Array<unsigned char> back;
    public:
        int read_byte () {
            return fgetc(handle);
        }
};
