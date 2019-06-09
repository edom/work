#ifndef MEMORY_H_INCLUDED
#define MEMORY_H_INCLUDED

#include <cstdlib>

struct Memory {
    struct Exception {
        size_t size;
    };

    virtual ~Memory ();

    virtual void* allocate_or_null (size_t) throw () = 0;

    void* allocate_or_throw (size_t size) {
        void* ptr = allocate_or_null(size);
        if (ptr == nullptr) {
            throw Exception({size:size});
        }
        return ptr;
    }

    virtual void release (void*) throw () = 0;

    virtual void release_all () throw () = 0;

    // including overhead such as fragmentation,
    // but not including the Memory instance itself
    virtual size_t count_bytes_used () {
        return 0;
    }
};

struct Memory_default : Memory {
};

#endif
