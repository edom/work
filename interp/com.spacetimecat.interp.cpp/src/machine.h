#ifndef MACHINE_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define MACHINE_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include "gc.h"
#include "std.h"

namespace Interp_Impl {

    class Value;
    class Error;

    struct Key final {
        // A char* points to a null-terminated string.
        // A byte* points to a byte string that is not necessarily null-terminated.
        using byte = unsigned char;

        static_assert(sizeof(byte) == 1, "sizeof(byte) == 1");

        size_t hash; // precomputed hash
        size_t length; // byte count
        byte* bytes; // constant through this object's lifetime; not null-terminated

        Key (const char* asciz);
        Key (const byte* bytes_, size_t length_);
        Key (const Key& that);
        Key (Key&& that) noexcept;
        ~Key ();

        int compare (const Key& that) const;

        bool operator== (const Key& that) const;
        bool operator!= (const Key& that) const;
        bool operator<= (const Key& that) const;
        bool operator< (const Key& that) const;

        Std_String to_std_string () const;
    };

    struct Key_Hasher {
        size_t operator() (const Key& that) const noexcept;
    };

    // TOOD frames of stacks; read from front, read from back, write from back
    class Machine final : public GC_Object {

        private:

            using Bindings = Std_Unordered_Map<Key, Value*, Key_Hasher>;

            Objects objects;
            Std_Vector<Value*> stack;
            Error* error = nullptr; // current exception
            Bindings bindings;

        public:

            Machine ();
            Machine (const Machine&) = delete;

            ~Machine ();

        // -------------------- bindings

            void    bind (const char* name, Value* value);
            void    bind (const Key& key, Value* value);
            void    bind (Key&& key, Value* value);

            Value*  lookup (const char* name) const;
            Value*  lookup (const Std_String& name) const;
            Value*  lookup (const Key& key) const;

            // Find all keys that begin with the prefix, for readline.
            Std_Vector<Std_String> find_keys_with_prefix (const char* prefix) const;

        // -------------------- stack

            void    push (Value* a);
            Value*  pop ();
            int     pop_int ();
            size_t  stack_size () const;
            void    print_stack () const;

        // -------------------- cooperative exception handling

            void    raise (Error* error_);
            void    clear_error ();
            Error*  get_error_or_null () const;

        // -------------------- miscellany

            void    load_standard_library ();

        // -------------------- memory management

        // ------------------------------ allocation

        public:

            template <typename T, typename ...Arg> T* new_ (Arg&&... arg) {
                return objects.new_<T,Arg...>(std::forward<Arg>(arg)...);
            }

        // ------------------------------ garbage collection

        public:

            void collect_garbage ();
            void show_gc_stats ();

        public:

            //  Internal.

            void mark_children () override;

    };
}

#endif
