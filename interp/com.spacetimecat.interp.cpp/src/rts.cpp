#include "rts.hpp"
#include <cassert>
#include <cstdio>
#include <cstdlib>

namespace stc_rts {

    Heap::Heap(Byte_Ptr base, N_Byte limit)
    : _base(base)
    , _limit(limit)
    , _next(base)
    { }

    Heap::~Heap() {
        _base = nullptr;
        _limit = 0;
        _next = nullptr;
    }

    Byte_Ptr Heap::end() const {
        return _base + _limit;
    }

    N_Byte Heap::count_bytes_limit() const { return _limit; }
    N_Byte Heap::count_bytes_used() const { return _next - _base; }
    N_Byte Heap::count_bytes_free() const { return count_bytes_limit() - count_bytes_used(); }

    static Byte_Ptr align_up (Byte_Ptr ptr, Align align) {
        std::uintptr_t unaligned = (std::uintptr_t) ptr;
        std::uintptr_t check = align - 1;
        //  Check that align is a power of 2.
        if ((align & check) != 0) { return nullptr; }
        std::uintptr_t mod = unaligned & check;
        std::uintptr_t aligned = unaligned + ((align - mod) & check);
        assert((aligned & check) == 0);
        return (Byte_Ptr) aligned;
    }

    //  align must be a power of 2.
    bool Heap::allocate(char*& ptr, N_Byte size, Align align) {
        Byte_Ptr aligned_base = align_up(_next, align);
        Byte_Ptr aligned_end = aligned_base + size;
        if (aligned_end > end()) { return false; }
        _next = aligned_end;
        ptr = aligned_base;
        return true;
    }



    World::World()
    : heap(nullptr, 0)
    , refs(Array<Ref>(nullptr, 0))
    , next_object_id(0)
    { }

    World::~World() {
    }

    void World::init_heap(void* base, N_Byte limit) {
        this->heap = Heap((char*) base, limit);
    }

    void World::init_refs(Ref* ptr, Count limit) {
        for (Count i = 0; i < limit; ++i) {
            ptr[i].ptr = nullptr;
            ptr[i].pinned = false;
        }
        refs = Array<Ref>(ptr, limit);
    }

    bool World::find_free_object_id(Object_Id& result) {
        Object_Id limit = count_refs_limit();
        Object_Id end = next_object_id;
        Object_Id i = end;
        bool found = false;
        do {
            if (nullptr == refs[i].ptr) {
                found = true;
            }
            ++i;
            if (i >= limit) { i = 0; }
            if (found) {
                result = i;
                next_object_id = i;
                break;
            }
        } while (i != end);
        return found;
    }

    bool World::new_byte_string(Object_Id& result, const void* orig_ptr, Size orig_size) {
        Object_Id   obj_id;
        char*       obj_ptr;
        char*       content_ptr;
        N_Byte      obj_size = sizeof(Byte_String) + orig_size;
        Align       obj_align = alignof(Byte_String);
        if (find_free_object_id(obj_id) && heap.allocate(obj_ptr, obj_size, obj_align)) {
            content_ptr = obj_ptr + obj_size;
            new (obj_ptr) Byte_String(*this, content_ptr, orig_size);
            std::memcpy(content_ptr, orig_ptr, orig_size);
            refs[obj_id] = Ref((Object*) obj_ptr);
            after_create_object();
            result = obj_id;
            return true;
        } else {
            return false;
        }
    }

    void World::after_create_object() {
        ++next_object_id;
        if (next_object_id >= count_refs_limit()) {
            next_object_id = 0;
        }
        ++n_refs_used;
    }

    Count World::count_refs_limit() const { return refs.limit(); }
    Count World::count_refs_used() const { return n_refs_used; }
    Count World::count_refs_free() const { return count_refs_limit() - count_refs_used(); }



    Object::Object(World& world) : _world(world) { }
    Object::~Object() { }
    void Object::begin_iterating_references() { }
    bool Object::get_next_reference(Object_Id&) { return false; }
    Byte_String* Object::as_byte_string() { return nullptr; }



    Byte_String::Byte_String(World& world, char* bytes, N_Byte count)
    : Object(world), _bytes(bytes), _count(count)
    { }
    Byte_String::~Byte_String() { }
    Byte_String* Byte_String::as_byte_string() { return this; }



    Const_Asciz::Const_Asciz (const char* ptr_) : ptr(ptr_) { }
    N_Byte Const_Asciz::length () const { return std::strlen(ptr); }



    Pair::Pair(World& world, Object_Id first, Object_Id second)
    : Object(world), _gc_trace_index(0) {
        _ids[0] = first;
        _ids[1] = second;
    }
    Pair::~Pair() { }
    void Pair::begin_iterating_references() {
        _gc_trace_index = 0;
    }
    bool Pair::get_next_reference(Object_Id& id) {
        if (_gc_trace_index >= 2) { return false; }
        id = _ids[_gc_trace_index];
        ++_gc_trace_index;
        return true;
    }
    Object_Id Pair::get_first() const { return _ids[0]; }
    Object_Id Pair::get_second() const { return _ids[1]; }



    int main(int argc, char* argv[]) {
        // begin:
            int ret = EXIT_SUCCESS;
            N_Byte heap_limit = 1048576;
            void* heap_base = std::malloc(heap_limit);
            Count ref_limit = 1048576;
            Ref* refs = (Ref*) std::malloc(ref_limit * sizeof(Ref));
            if (nullptr == heap_base) { goto error_malloc; }
            if (nullptr == refs) { goto error_malloc; }
            {
                World world;
                world.init_heap(heap_base, heap_limit);
                world.init_refs(refs, ref_limit);
                Object_Id oid;
                const char* str = "foo";
                if (world.new_byte_string(oid, str, 1 + std::strlen(str))) {
                    std::puts("OK");
                }
            }
            goto end;
        error_malloc:
            std::perror("malloc");
            ret = EXIT_FAILURE;
            goto end;
        end:
            if (nullptr != heap_base) { std::free(heap_base); }
            if (nullptr != refs) { std::free(refs); }
            return EXIT_SUCCESS;
    }

}
