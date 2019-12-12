#ifndef STC_RTS_HPP_INCLUDED
#define STC_RTS_HPP_INCLUDED

#include <cctype>

//  Constraints:
//
//      -   Cannot use C++ STL.
//      -   Cannot use C++ exceptions.
//      -   Cannot use libc.
//      -   Cannot use libstdc++.

namespace stc_rts {

    using Count = std::size_t;
    using N_Byte = std::size_t;

    //  STL replacement.

    template<typename T> class Array {
        private:
            T* _ptr;
            Count _limit;
        public:
            Array(T* ptr_, Count limit_) : _ptr(ptr_), _limit(limit_) {}
            ~Array() { _ptr = nullptr; _limit = 0; }
            Count limit () const { return _limit; }
            T& operator[](Count index) const { return _ptr[index]; }
            T& operator[](Count index) { return _ptr[index]; }
    };

    struct Const_Asciz {
        const char* ptr;
        Const_Asciz(const char*);
        N_Byte length() const;
    };

    //  Runtime system.

    using Byte_Ptr = char*;
    using Size = std::size_t;
    using Align = std::size_t;
    using Object_Id = std::uint32_t;

    class Heap {
        private:
            Byte_Ptr _base;
            N_Byte _limit;
            Byte_Ptr _next;
        public:
            Heap(Byte_Ptr base, N_Byte limit);
            ~Heap();
            bool allocate(char*& ptr, N_Byte size, Align align);
            Byte_Ptr end() const;
            //  Statistics.
            N_Byte count_bytes_limit() const;
            N_Byte count_bytes_used() const;
            N_Byte count_bytes_free() const;
            //  There is no free. Use copying garbage collection.
    };

    class Object;

    struct Ref {
        Object* ptr;
        bool pinned;

        Ref() : Ref(nullptr, false) {}
        Ref(Object* ptr_) : Ref(ptr_, false) {}
        Ref(Object* ptr_, bool pinned_) : ptr(ptr_), pinned(pinned_) {}
    };

    class World {
        private:
            Heap heap;
            Array<Ref> refs;
            Object_Id next_object_id;
            Count n_refs_used;
            bool find_free_object_id(Object_Id&);
            void after_create_object();
        public:
            World();
            ~World();
            void init_heap(void* ptr, N_Byte limit);
            void init_refs(Ref* ptr, Count limit);
            bool new_byte_string(Object_Id&, const void*, Size);
            Count count_refs_limit() const;
            Count count_refs_used() const;
            Count count_refs_free() const;
    };

    class Byte_String;

    class Object {
        protected:
            World& _world;
        public:
            Object(World&);
            virtual ~Object();
            //  Garbage collection.
            virtual void begin_iterating_references();
            virtual bool get_next_reference(Object_Id&);
            //  Casting.
            virtual Byte_String* as_byte_string();
    };

    class Byte_String : public Object {
        private:
            char* _bytes;
            N_Byte _count;
        public:
            Byte_String(World&, char*, N_Byte);
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
            Pair(World&, Object_Id, Object_Id);
            ~Pair() override;
            void begin_iterating_references() override;
            bool get_next_reference(Object_Id&) override;
            Object_Id get_first() const;
            Object_Id get_second() const;
    };

    int main(int argc, char* argv[]);
};

#endif
