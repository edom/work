#include <cstdlib>
#include <cstdio>

//  This is going to conflict if the RTS is embedded in other C++ programs.
//  This should only be linked into applications and not libraries.

//  Why does destructors require operator delete even though we don't use it anywhere?
void operator delete(void*) {
    std::fprintf(stderr, "delete not implemented\n");
    std::abort();
}
