#ifndef TERM_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define TERM_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include <cinttypes>
#include <cstdarg>
#include <cstdint>
#include <cstdio>

#include "../array.h"

namespace Interp_Prolog {

    using Interp_Impl::GC_Object;

    // -------------------- begin questionable code

    //  These Object-like classes are not used anywhere.
    //  Why do we have them?
    //  These are only sketches.

    //  Garbage-collectible foreign object.

    class Foreign_Object : public GC_Object {

        protected:

            void* raw;

        public:

            Foreign_Object (void* raw_);

            virtual ~Foreign_Object ();

    };

    //  C++ Foreign_Object.

    template<typename T>
    class Foreign final : public Foreign_Object {

        public:

            Foreign (T* raw_) : Foreign_Object(raw_) { }

            virtual ~Foreign () {
                if (raw != nullptr) {
                    delete (T*) raw;
                    raw = nullptr;
                }
            }

    };

    //  Garbage-collectible Array of Object pointers.
    //  T must be a subclass of GC_Object.

    template<typename T>
    class Array_Object final : public GC_Object, public Array<T*> {

        public:

            void mark_children () override {
                for (size_t i = 0; i < this->count_; ++i) {
                    if (this->items[i] == nullptr) { continue; }
                    this->items[i]->mark();
                }
                GC_Object::mark_children();
            }

    };

    // -------------------- end questionable code

    class Integer;
    class String;
    class Compound;
    class Var;

    class Term : public GC_Object {

        public:

            //  Destructor should be accessed by garbage collection.

            virtual ~Term () {}

            // -------------------- type-checking

            //  Check whether this Term is an instance of Var.
            //
            //  Note that a bound variable is still an instance of Var,
            //  even though it dereferences to a non-Var.

            virtual bool is_Var () const;

            //  This is only valid if is_Var() is true.

            virtual bool is_unbound () const;

            // -------------------- type-casting and pattern-matching

            virtual bool get_compound (const Compound** val) const;
            virtual bool get_integer (intptr_t* val) const;
            virtual bool get_string (const String** val) const;

            //  Consider using as_Var_or_null instead.

            virtual bool get_var (const Var** val) const;
            virtual bool get_var (Var** val);

            virtual Var*        as_Var_or_null ();
            virtual const Var*  as_Var_or_null () const;

            // -------------------- graph

            //  Get ultimate referent, transitively, until it is impossible to dereference.

            virtual Term*       dereference ();
            virtual const Term* dereference () const;

            // -------------------- debug

            void print_debug (String* out) const;

        protected:

            virtual void do_print_debug (String* out) const = 0;

    };

    // mutable byte string
    // TODO make Array extend Term?
    // move this to Interp_Impl
    // Make an Interp_Impl::Prolog_String that extends Interp_Impl::String

    class String final : public Term, private Array<char> {

        public:

            // -------------------- construction

            String ();
            String (size_t limit);
            String (const char* s);

            static String* copy_cstr (const char* cstr) {
                size_t limit = strlen(cstr);
                String* s = new String(limit);
                s->append_trunc(cstr);
                return s;
            }

            String* copy () const {
                String* c = new String(count_);
                for (size_t i = 0; i < count_; ++i) {
                    c->items[i] = items[i];
                }
                c->count_ = count_;
                return c;
            }

            // -------------------- delegates

            using Array::clear;
            using Array::write_to;

            // -------------------- comparison

            bool equals (const String* that) const {
                assert(this->count_ <= this->limit);
                assert(that->count_ <= that->limit);
                if (this->count_ != that->count_) {
                    return false;
                }
                size_t i = 0;
                while (i < count_) {
                    if ((*this)[i] != (*that)[i]) {
                        return false;
                    }
                    ++i;
                }
                return true;
            }

            // -------------------- append

            size_t append_trunc (const String* that) {
                size_t i = 0;
                while (count_ < limit && i < that->count_) {
                    (*this)[count_] = (*that)[i];
                    ++count_;
                    ++i;
                }
                return i;
            }

            size_t append_trunc (const char* cstr) {
                size_t i = 0;
                while (count_ < limit && cstr[i] != 0) {
                    (*this)[count_] = cstr[i];
                    ++count_;
                    ++i;
                }
                return i;
            }

            void printf (const char* format, ...) {
                va_list ap;
                va_start(ap, format);
                size_t n = vsnprintf(items + count_, limit - count_, format, ap);
                count_ += n;
                if (count_ > limit) {
                    puts("String.printf: count_ > limit");
                    abort();
                }
                va_end(ap);
            }

            // -------------------- Term

            bool get_string (const String** val) const override {
                *val = this;
                return true;
            }

        protected:

            void do_print_debug (String* out) const override {
                out->append_trunc(this);
            }

    };

    // TODO replace with or extend Interp_Impl::Integer instead
    // Immutable integer with lots of overhead.
    class Integer final : public Term {

        private:

            const intptr_t i;

        public:

            Integer (intptr_t i_) : i(i_) { }

            bool get_integer (intptr_t* val) const override {
                *val = i;
                return true;
            }

        protected:

            void do_print_debug (String* out) const override {
                out->printf("%" PRIdPTR, i);
            }

    };

    class Compound final : public Term {

        public:

            String* name;
            size_t arity;
            Term** args;

            Compound (String* name, size_t arity, Term** args) {
                this->name = name;
                this->arity = arity;
                this->args = args;
            }

            bool get_compound (const Compound** val) const override {
                *val = this;
                return true;
            }

            void mark_children () override {
                name->mark();
                Term::mark_children();
                // TODO make args an Array, and make Array an Object
            }

        protected:

            void do_print_debug (String* out) const override {
                out->append_trunc(name);
                out->printf("(");
                for (size_t i = 0; i < arity; ++i) {
                    args[i]->print_debug(out);
                    if (i+1 < arity) {
                        out->printf(",");
                    }
                }
                out->printf(")");
            }

    };

    class Var final : public Term {

        friend class Frame;

        private:

            Term* v;

        public:

            Var ();
            Var (Term* v_);

            virtual ~Var ();

            bool is_Var () const override;
            bool is_unbound () const override;
            bool get_var (const Var** v) const override;
            bool get_var (Var** v) override;

            Var*        as_Var_or_null () override;
            const Var*  as_Var_or_null () const override;

            const Term* dereference () const override;

            Term* dereference () override;

            void mark_children () override;

        protected:

            void do_print_debug (String* out) const override;

    };

}

#endif
