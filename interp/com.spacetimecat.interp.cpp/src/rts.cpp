#include "rts.hpp"
#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <new>

namespace stc_rts {

//  This is the RTS flavor that depends on libc.

//  Problem: printf-family procedures may allocate memory.

[[noreturn]]
void panic(const char* format...) {
    FILE*   stream = stderr;
    va_list args;
    va_start(args, format);
    std::vfprintf(stream, format, args);
    std::fflush(stream);
    va_end(args);
    std::terminate();
}

void* malloc(Size size) { return std::malloc(size); }

void free(void* ptr) {
    //  For non-compliant implementations of free(2).
    if (nullptr != ptr) { std::free(ptr); }
}

Address::Address() { }
Address::Address(std::uintptr_t raw) : _raw(raw) { }
Address::Address(void* raw) : Address(reinterpret_cast<std::uintptr_t>(raw)) { }

Address& Address::operator=(void* that) {
    _raw = reinterpret_cast<std::uintptr_t>(that);
    return *this;
}

Address         Address::operator+(std::uintptr_t offset) const { return Address(this->_raw + offset); }
std::uintptr_t  Address::operator-(const Address& that) const { return this->_raw - that._raw; }
bool            Address::operator<(const Address& that) const { return this->_raw < that._raw; }
bool            Address::operator<=(const Address& that) const { return this->_raw <= that._raw; }
bool            Address::operator>(const Address& that) const { return this->_raw > that._raw; }

//  If align is not a power of 2, the result is undefined.
Address Address::aligned_up (Align align) const {
    std::uintptr_t unaligned = _raw;
    std::uintptr_t check = align - 1;
    if ((align & check) != 0) { return Address(nullptr); }
    std::uintptr_t mod = unaligned & check;
    std::uintptr_t aligned = unaligned + ((align - mod) & check);
    assert((aligned & check) == 0);
    return Address(aligned);
}

bool Address::is_null() const { return nullptr == to_ptr<void>(); }

Block::Block(Address base, N_Byte size) : _base(base), _size(size) { }

Address Block::begin() const { return _base; }
Address Block::end() const { return _base + _size; }
N_Byte  Block::size() const { return _size; }
bool Block::is_null() const { return _base.is_null(); }

bool Ref::is_free() const { return nullptr == _ptr; }

Object* Tmp_Ref::operator->() const {
    return _ref->_ptr;
}

Heap::Heap(const Block& block, const Array<Ref>& refs)
: _block(block)
, _refs(refs)
, _next(block.begin())
, _next_object_id(0)
{
    for (Count i = 0; i < _refs.limit(); ++i) {
        _refs[i] = Ref(nullptr, 0);
    }
}

Address Heap::begin() const { return _block.begin(); }
Address Heap::end() const { return _block.end(); }
bool Heap::failed() const { return _failed; }
N_Byte Heap::count_bytes_limit() const { return _block.size(); }
N_Byte Heap::count_bytes_used() const { return _next - begin(); }
N_Byte Heap::count_bytes_free() const { return count_bytes_limit() - count_bytes_used(); }

Tmp_Ref Heap::get_temporary_reference(Object_Id id) {
    return Tmp_Ref(_refs[id]);
}

//  align must be a power of 2.
bool Heap::allocate_bytes(Address& result, N_Byte size, Align align) {
    Address aligned_begin = _next.aligned_up(align);
    Address block_end = aligned_begin + size;
    if (block_end <= end()) {
        _next = block_end;
        result = aligned_begin;
        return true;
    } else {
        _failed = true;
        return false;
    }
}

bool Heap::allocate_object_id(Object_Id& result) {
    Object_Id   limit = count_refs_limit();
    Object_Id   patience = limit;
    Object_Id   suspect = _next_object_id;
    bool        found = false;
    for (patience = limit; patience > 0; --patience) {
        if (_refs[suspect].is_free()) {
            found = true;
            result = suspect;
        }
        if (++suspect >= limit) { suspect = 0; }
        if (found) {
            _next_object_id = suspect;
            break;
        }
    }
    if (!found) { _failed = true; }
    return found;
}

bool Heap::allocate_object(Object_Id& out_id, Address& out_addr, N_Byte size, Align align) {
    if (allocate_object_id(out_id) && allocate_bytes(out_addr, size, align)) {
        _refs[out_id] = Ref(out_addr.to_ptr<Object>());
        return true;
    } else {
        return false;
    }
}

Count Heap::count_refs_limit() const { return _refs.limit(); }
Count Heap::count_refs_free() const { return count_refs_limit() - count_refs_used(); }
Count Heap::count_refs_used() const {
    Count n = count_refs_limit();
    Count used = 0;
    for (Object_Id i = 0; i < n; ++i) {
        if (!_refs[i].is_free()) {
            ++used;
        }
    }
    return used;
}



World::World(Heap& heap)
: _heap(heap)
{
    Address address;
    heap.allocate_object(_error_object, address, 1, 1);
}

World::~World() { }

bool World::failed_to_allocate() const { return _heap.failed(); }

bool World::new_byte_string(Object_Id& result, const void* orig_ptr, Size orig_size) {
    Object_Id   obj_id;
    Address     obj_addr;
    N_Byte      obj_size = sizeof(Byte_String) + orig_size;
    Align       obj_align = alignof(Byte_String);
    if (_heap.allocate_object(obj_id, obj_addr, obj_size, obj_align)) {
        char*   obj_ptr = obj_addr.to_ptr<char>();
        char*   content_ptr = obj_ptr + obj_size;
        new (obj_ptr) Byte_String(content_ptr, orig_size);
        std::memcpy(content_ptr, orig_ptr, orig_size);
        result = obj_id;
        return true;
    } else {
        return false;
    }
}
bool World::new_byte_string_from_asciz(Object_Id& result, const char* str) {
    return new_byte_string(result, str, 1 + std::strlen(str));
}
Tmp_Ref World::get_temporary_reference(Object_Id id) {
    return _heap.get_temporary_reference(id);
}



Object::Object() { }
Object::~Object() { }
void Object::begin_iterating_references() { }
bool Object::get_next_reference(Object_Id&) { return false; }
Byte_String* Object::as_byte_string() { return nullptr; }



Byte_String::Byte_String(char* bytes, N_Byte count)
: Object(), _bytes(bytes), _count(count)
{ }
Byte_String::~Byte_String() { }
Byte_String* Byte_String::as_byte_string() { return this; }
char* Byte_String::bytes() const { return _bytes; }
N_Byte Byte_String::length() const { return _count; }



Const_Asciz::Const_Asciz (const char* ptr_) : ptr(ptr_) { }
N_Byte Const_Asciz::length () const { return std::strlen(ptr); }



Pair::Pair(Object_Id first, Object_Id second)
: Object(), _gc_trace_index(0) {
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
Object_Id Pair::get_first_id() const { return _ids[0]; }
Object_Id Pair::get_second_id() const { return _ids[1]; }

}
