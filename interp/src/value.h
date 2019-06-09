#ifndef VALUE_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define VALUE_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include "gc.h"

#if WANT_GMP
#include "integer_gmp.h"
#else
#include "integer_std.h"
#endif

namespace Interp_Impl {

    struct Type {
        Std_String name;
    };

    class Machine;
    class Integer;

    // We must know the type, because we want polymorphism,
    // because we want to pretty-print the stack.
    class Value : public GC_Object {
        friend class Objects;
        friend class Machine;

        protected:
            Value () {}

        public:
            virtual ~Value () {}
            virtual Type* type () const = 0;
            virtual void throw_if_error () const { }

            virtual Integer*
            be_integer_or_null () { return nullptr; }

            Integer*
            be_integer_or_throw ();

            virtual bool
            get_int (int& out) const {
                (void) out;
                return false;
            }

            virtual Std_String to_std_string () const {
                std::ostringstream s;
                s << type()->name << "@" << this;
                return s.str();
            }

            virtual void mutate (Machine& m);
    };

    class Integer : public Value {
        friend class Objects;
        private:
            using Big = ::Interp_Integer::Integer;

            Big i;

        private:
            Integer (const Big& that) : i(that) { }
            Integer (Big&& that) : i(that) { }

        protected:
            Integer (long i_) : i(i_) {}
            Integer (const Integer& that) : i(that.i) {}
            Integer (const char* s) : i(s) {}
            Integer (const Std_String& s) : i(s) {}

        public:
            Integer* be_integer_or_null () override { return this; }

            Integer operator+ (const Integer& that) { return Integer(this->i + that.i); }
            Integer operator- (const Integer& that) { return Integer(this->i - that.i); }
            Integer operator* (const Integer& that) { return Integer(this->i * that.i); }

            Type* type () const override;

            bool get_int (int& out) const override {
                return i.to_int(out);
            }

            int get_int_or_throw () const {
                int i;
                if (get_int(i)) {
                    return i;
                }
                throw std::out_of_range("Integer");
            }

            Std_String to_std_string () const override { return i.to_std_string(); }
    };

    // Other names: applicable, executable, callable, subroutine, mutator, transition, runnable, step, endofunction, manipulator
    class Operation : public Value {
        friend class Objects;
        public:
            using Function = void(Machine&);

        protected:
            Function* imp;

            Operation (Function* imp_);

        public:
            Type* type () const override;
            void mutate (Machine& m) override;
    };

    class String : public Value {
        public:
            Std_String s;

            String (const Std_String& s_) : s(s_) { }
            String (const char* s_) : s(s_) { }

            Type* type () const override;
    };

    class Error : public Value, public std::runtime_error {
        public:
            Error (const Std_String& message_)
            : runtime_error(message_)
            { }

            void throw_ () const { throw *this; }
            void throw_if_error () const override { throw_(); }
            Type* type () const override;

            Std_String message () const {
                return what();
            }

            Std_String to_std_string () const override {
                std::ostringstream s;
                s << "Error[" << what() << "]";
                return s.str();
            }

        private:
            using runtime_error::what;
    };

    namespace Types {
        extern Type Integer;
        extern Type String;
        extern Type Operation;
        extern Type Error;
    }

    namespace Errors {
        extern Error type_error;
        extern Error key_not_found;
        extern Error stack_underflow;
    }

}

#endif
