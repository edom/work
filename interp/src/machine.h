#ifndef MACHINE_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define MACHINE_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include "bindings.h"
#include "gc.h"

namespace Machine {

    class Value;
    class Error;

    struct State {
        virtual ~State () {}
        virtual void push (Value*) = 0;
        virtual Value* pop () = 0;
    };

    // TOOD frames of stacks; read from front, read from back, write from back
    class Machine : public GC_Object, public State {
        private:
            Objects objects;
            std::vector<Value*> stack;
            Error* error = nullptr; // current exception

        public:
            Bindings bindings;

            virtual ~Machine ();

            void bind (const char* name, Value* value);

            void load_standard_library ();

            Value* lookup (const Key& key) const;
            Value* lookup (const char* name) const;
            Value* lookup (const std::string& name) const;

            void push (Value* a) override;
            Value* pop () override;
            int pop_int ();

            // Cooperative exception handling.
            void raise (Error* error_) {
                this->error = error_;
            }

            void clear_error () {
                this->error = nullptr;
            }

            Error* get_error_or_null () const {
                return error;
            }

            size_t stack_size () const {
                return stack.size();
            }

            void print_stack () const;

        // -------------------- memory management

        public:
            // Instantiate and track for garbage collection.
            // The object will be garbage-collected in the next call of collect_garbage,
            // unless it is reachable from the stack or the bindings.
            template <typename T, typename ...Args> T* new_ (Args&&... args) {
                // How do we explain this incantation?
                T* inst = new T(std::forward<Args>(args)...);
                Value* dummy = inst; // T must be a subtype of Value
                (void) dummy;
                track(inst);
                return inst;
            }

        private:
            // Register the object for automatic memory management by garbage collection.
            void track (GC_Object* object);
            void collect_garbage ();
            void mark () override;
            void show_gc_stats ();
    };
}

#endif
