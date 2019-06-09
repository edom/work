#include <cstdarg>
#include <cstdint>

#include "../array.h"

namespace Interp_Prolog {

    class Integer;
    class String;
    class Compound;
    class Var;

    class Term : public GC_Object {

        public:

            // Destructor should be accessed by garbage collection.
            virtual ~Term () {}

            // type-checking and pattern-matching

            virtual bool is_var () const { return false; }
            virtual bool is_unbound () const { return false; }
            virtual bool get_compound (const Compound** val) const { (void) val; return false; }
            virtual bool get_integer (intptr_t* val) const { (void) val; return false; }
            virtual bool get_string (const String** val) const { (void) val; return false; }
            virtual bool get_var (const Var** val) const { (void) val; return false; }
            virtual bool get_var (Var** val) { (void) val; return false; }

            // graph

            virtual const Term* dereference () const { return this; }
            virtual Term* dereference () { return this; }

            // debug

            void print_debug (String* out) const {
                const Term* t = dereference();
                t->do_print_debug(out);
            }

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
            String () : Array(128) {}
            String (size_t limit) : Array(limit) {}
            String (const char* s)
            : Array(strlen(s)) {
                // inlined from copy_cstr
                append_trunc(s);
            }
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
            void clear () {
                return Array::clear();
            }
            size_t write_to (FILE* out) const {
                return Array::write_to(out);
            }
//            using clear = Array::clear;
//            using write_to = Array::write_to;
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
            Var () : v(nullptr) {}
            Var (Term* v_) : v(v_) {}
            virtual ~Var () { v = nullptr; }
            bool is_var () const override { return true; }
            bool is_unbound () const override {
                const Term* t = dereference();
                const Var* var;
                return t->get_var(&var) && var == nullptr;
            }
            bool get_var (const Var** v) const override {
                *v = this;
                return true;
            }
            bool get_var (Var** v) override {
                *v = this;
                return true;
            }
            // get direct/immediate/proximate referent, not ultimate/transitive
            const Term* dereference () const override {
                if (v == nullptr) { return this; }
                return v->dereference();
            }
            Term* dereference () override {
                if (v == nullptr) { return this; }
                return v->dereference();
            }
            void mark_children () override {
                if (v != nullptr) { v->mark(); }
                Term::mark_children();
            }
        protected:
            void do_print_debug (String* out) const override {
                out->printf("_%p", this);
            }
    };
}
