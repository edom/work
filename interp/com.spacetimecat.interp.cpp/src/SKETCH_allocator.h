#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <memory>
#include <vector>

struct Allocator {

    template<typename T>
    T* allocate (size_t n_elem) {
        size_t n_byte = n_elem * sizeof(T);
        void* ptr = malloc(n_byte);
        if (ptr == nullptr) { abort(); }
        return (T*)ptr;
    }

    void deallocate (void* ptr) {
        free(ptr);
    }
};

Allocator DEFAULT_ALLOCATOR;

template <typename T, typename A = std::allocator<T> >
// struct Array2 : std::vector<T,A> { // ???
struct Array2 {
    T* ptr;
    size_t limit;
    size_t count;

    Array2 (T* ptr_, size_t limit_) : ptr(ptr_), limit(limit_), count(0) { }

    bool is_full () const { return count >= limit; }

    int add (T item) {
        if (is_full()) { return ENOMEM; }
        ptr[count] = item;
        ++count;
        return 0;
    }

    void set_backing (T* ptr, size_t limit) {
        this->ptr = ptr;
        this->limit = limit;
    }
};

struct read_line_asciz final {
    FILE* file;
    char* buffer;
    size_t limit;
    size_t length;

    read_line_asciz (FILE* file_, char* buffer_, size_t limit_)
    : file(file_)
    , buffer(buffer_)
    , limit(limit_)
    , length(0)
    { }

    void set_buffer (char* buffer, size_t limit) {
        this->buffer = buffer;
        this->limit = limit;
    }

    int step () {
        if (length >= limit) {
            return ENOMEM;
        }
        int c = fgetc(file);
        if (c == EOF || c == '\n') {
            buffer[length] = 0;
            return 0;
        }
        buffer[length] = c;
        ++length;
        return EAGAIN;
    }
};

int main (int argc, char* argv[]) {
    Allocator a;
    std::vector<int> z;
    read_line_asciz c0 (stdin, a.allocate<char>(4), 4);
    for (;;) {
        int r0 = c0.step();
        if      (r0 == 0) { break; }
        else if (r0 == EAGAIN) { }
        else if (r0 == ENOMEM) {
            size_t limit = 2 * c0.limit;
            if (limit <= c0.limit) { abort(); }
            c0.set_buffer((char*)realloc(c0.buffer, limit), limit);
        }
    }
    printf("%s\n", c0.buffer);
    a.deallocate(c0.buffer);
    return EXIT_SUCCESS;
}
