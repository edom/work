#ifndef STC_RTS_HPP_INCLUDED
#define STC_RTS_HPP_INCLUDED

//  I want the run-time system (the RTS) to be able to run on bare metal,
//  so the RTS should not require linking with libc, let alone libstdc++.
//
//  See also:
//
//      -   http://ptspts.blogspot.com/2010/12/how-to-write-c-program-without-libstdc.html
//
//  Constraints:
//
//      -   Cannot use C++ STL containers.
//      -   Cannot use C++ exceptions.
//      -   Cannot use libc.
//      -   Cannot use libstdc++.

//  Table of contents:
//      Integral types
//      Platform abstractions
//      Memory management
//          Address
//          Block
//          Heap
//          Ref
//          Tmp_Ref
//          World
//      STL-like functionality
//          Array: fixed-size random-access homogenous collection

#include <cstdint>

namespace stc_rts {

//  --------------------    Integral types.

using Align = std::size_t;  //  should be a power of 2
using N_Byte = std::size_t; //  number of bytes
using Count = std::size_t;  //  number of things of various sizes
using Length = std::size_t; //  number of things of the same size
using Size = std::size_t;   //  number of bytes; alias of std::size_t

//  --------------------    Platform abstractions.

//  The difficulty of porting the RTS is proportional to the number of things defined here.

//  Call this instead of calling abort.

[[noreturn]]
void    panic(const char* format...);

//  malloc and free have the same semantics as std::malloc and std::free, respectively.

void*   malloc(Size);
void    free(void*);

//  --------------------    Memory management.

//  An Address wraps an integer for unchecked pointer arithmetic and casting.

class Address {
    private:
        std::uintptr_t _raw;
    public:
        Address();
        //  Should these constructors be explicit?
        Address(std::uintptr_t);
        Address(void*);

        template <typename T>
        T* to_ptr() const { return reinterpret_cast<T*>(_raw); };

        Address&        operator=(void*);
        Address         operator+(std::uintptr_t) const;
        std::uintptr_t  operator-(const Address&) const;
        bool            operator<(const Address&) const;
        bool            operator<=(const Address&) const;
        bool            operator>(const Address&) const;

        Address aligned_up(Align align) const;
        bool    is_null() const;
};

//  A Block is a contiguous address range.

class Block {
    private:
        Address     _base;
        N_Byte      _size;
    public:
        Block(Address base, N_Byte size);
        Address     begin() const;
        Address     end() const;
        N_Byte      size() const;

        bool is_null() const;
};

//  --------------------    STL-like functionality.

//  An array is a fixed-size random-access homogenous collection.
//  The caller is responsible for allocation.
//
//  This is like std::array, but this does not allocate memory on its own.

template <typename T>
class Array {
    private:
        T*      _ptr;
        Count   _limit;
    public:
        Array(T* ptr_, Count limit_) : _ptr(ptr_), _limit(limit_) {}
        ~Array() { _ptr = nullptr; _limit = 0; }
        T* begin() const { return _ptr; }
        T* end() const { return _ptr + _limit; }
        Count limit() const { return _limit; }
        T& operator[](Count index) const { return _ptr[index]; }
        T& operator[](Count index) { return _ptr[index]; }
        bool is_null() const { return nullptr == _ptr; }
};

struct Const_Asciz {
    const char* ptr;
    Const_Asciz(const char*);
    N_Byte length() const;
};

//  We assume that there is never too many live objects while the program is running.

using Object_Id = std::uint32_t;

class Ref;
class Tmp_Ref;

//  This Heap is a combination of a memory block and an array of object references.
//  We assume that deallocation is done by copying garbage collection outside this Heap.
//
//  The caller is responsible for the proportion between bytes and slots.
//  This is like free space vs inode in the Extended File System.
//
//  Related terms: arena, nursery, pool.
//
//  Allocation failure makes "failed" return true.

class Heap {
    private:
        Block       _block;
        Array<Ref>  _refs;
        Address     _next;
        Object_Id   _next_object_id;
        bool        _failed;
    public:
        Heap(const Block& block, const Array<Ref>& refs);
    private:
        bool        allocate_bytes(Address& address, N_Byte size, Align align);
        bool        allocate_object_id(Object_Id&);
    public:
        bool        failed() const;
        bool        allocate_object(Object_Id& id, Address& address, N_Byte size, Align align);
        Tmp_Ref     get_temporary_reference(Object_Id);
        template <typename T, typename...Args>
        T* new_object (Args...args) {
            Object_Id id;
            Address addr;
            allocate_object(id, addr, sizeof(T), alignof(T));
            T* ptr = addr.to_ptr<T>();
            if (nullptr != ptr) {
                new (ptr) T(args...);
            }
            return ptr;
        }
    private:
        Address     begin() const;
        Address     end() const;
    public:
        //  Statistics.
        N_Byte      count_bytes_limit() const;
        N_Byte      count_bytes_used() const;
        N_Byte      count_bytes_free() const;
        Count       count_refs_limit() const;
        Count       count_refs_used() const;
        Count       count_refs_free() const;
};

class Object;

//  A slot in the heap.
//  This is like an inode in the Extended File System.

class Ref {
    friend class Heap;
    friend class Tmp_Ref;
    private:
        Object* _ptr;
        int     _users;
    public:
        Ref() : Ref(nullptr, 0) {}
        Ref(Object* ptr) : Ref(ptr, 0) {}
        Ref(Object* ptr, int users) : _ptr(ptr), _users(users) {}
        bool is_free() const;
        void acquire() { ++_users; }
        void release() { --_users; }
};

//  Users should use "auto" instead of "Tmp_Ref" in their local variable declarations.
//
//  This behaves like an Object* (a pointer to an Object).
//  This temporarily disables garbage collection.

class Tmp_Ref {
    friend class Heap;
    private:
        Ref* _ref;
        Tmp_Ref(Ref& ref) : _ref(&ref) {
            _ref->acquire();
        }
    public:
        Tmp_Ref(Tmp_Ref&& that) {
            this->_ref = that._ref;
            that._ref = nullptr;
        }
        ~Tmp_Ref() {
            if (nullptr != _ref) {
                _ref->release();
                _ref = nullptr;
            }
        }
        //  The caller must not store the returned pointer.
        //  The returned pointer may be moved/invalidated by garbage collection.
        //  The returned pointer must not outlive this Tmp_Ref.
        Object* operator->() const;
};

//  Heap deals with memory blocks.
//  World deals with Object instances.

class World {
    public:
    private:
        Heap&       _heap;
        Object_Id   _error_object;
    public:
        World(Heap&);
        ~World();
        bool failed_to_allocate() const;
        bool new_byte_string(Object_Id&, const void*, Size);
        bool new_byte_string_from_asciz(Object_Id&, const char*);
        Tmp_Ref get_temporary_reference(Object_Id id);
};

class Byte_String;

class Object {
    public:
        Object();
        virtual ~Object();
        //  Garbage collection.
        virtual void begin_iterating_references();
        virtual bool get_next_reference(Object_Id&);
        //  Casting.
        virtual Byte_String* as_byte_string();
};

//  This is similar to Block, but uses char* instead of Address.
class Byte_String : public Object {
    private:
        char* _bytes;
        N_Byte _count;
    public:
        Byte_String(char*, N_Byte);
        ~Byte_String() override;
        char* bytes() const;
        N_Byte length() const;
        Byte_String* as_byte_string() override;
};

class Pair : public Object {
    private:
        Object_Id _ids[2];
        int _gc_trace_index;
    public:
        Pair(Object_Id, Object_Id);
        ~Pair() override;
        void begin_iterating_references() override;
        bool get_next_reference(Object_Id&) override;
        //  Trade-off: Returning Object_Id simplifies the implementation but complicates the usage.
        Object_Id get_first_id() const;
        Object_Id get_second_id() const;
        //  But we can add a World& field and add write a "get_first" that returns a Tmp_Ref.
};

class Platform {
    public:
        virtual ~Platform();
        virtual Address allocate(Size) = 0;
        virtual void free(Address) = 0;
};

}

#endif
