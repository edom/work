#include <cassert>
#include <cstdlib>
#include "rts.hpp"
#include "pl0.hpp"

namespace stc_rts {

void free(Address addr) {
    free(addr.to_ptr<void>());
}

Block allocate_block(N_Byte size) {
    return Block(malloc(size), size);
}

template <typename T>
Array<T> allocate_array(Count count) {
    return Array<T>(
        static_cast<T*>(malloc(sizeof(T) * count))
      , count
    );
}

void free_block(Block& block) {
    free(block.begin());
}

template <typename T>
void free_array(Array<T>& array) {
    free(array.begin());
}
}

int main(int argc, char* argv[]) {
    using namespace stc_rts;
    using namespace stc_pl0;
    // begin:
        int         ret = EXIT_SUCCESS;
        Block       block = allocate_block(1048576);
        Array<Ref>  refs = allocate_array<Ref>(1048576);
        if (block.is_null()) { goto error_malloc; }
        if (refs.is_null()) { goto error_malloc; }
        {
            Heap        heap(block, refs);
            World       world(heap);
            Object_Id   oid;
            C_Stream    stream("../src/example.txt", "rb");
            Parser      parser(world, stream);
            if (world.new_byte_string_from_asciz(oid, "foo")) {
                auto tmp_ref = world.get_temporary_reference(oid);
                Byte_String* bstr = tmp_ref->as_byte_string();
                std::printf("OK %p %s\n", bstr, bstr->bytes());
            }
        }
        goto end;
    error_malloc:
        std::perror("malloc");
        ret = EXIT_FAILURE;
        goto end;
    end:
        free_block(block);
        free_array(refs);
        return ret;
}
