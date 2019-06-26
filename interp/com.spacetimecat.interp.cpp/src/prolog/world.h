#ifndef WORLD_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define WORLD_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "term.h"

namespace Interp_Prolog {

    struct Binding final {
        Var* var;
        Term* ori;
    };

    //  The purpose of a frame is to undo a unification.

    class Frame final : public GC_Object, private Array<Binding> {

        friend class World;

        public:

            Frame (size_t limit);

            void save_once (Var* var);

            void set (Var* var, Term* val);

        public:

            void restore_to (size_t sp);

            // stack pointer
            size_t get_sp () const;

            void restore ();

            bool unify (Term* a, Term* b);

            // TODO override mark_children

    };

    class Clause final {
        private:
            Term* head;
            Term* body;
        public:
            Clause (Term* head_ = nullptr, Term* body_ = nullptr)
            : head(head_)
            , body(body_)
            {
            }
    };

    class World;

    //  Prolog "thread" (not OS thread)

    class Fiber : public GC_Object {

        friend class Call;

        private:

            World* world;
            Frame frame;
            Array<size_t> sp_stack;

        public:

            Fiber (World* world_);

            void push_frame ();

            void pop_frame ();

            bool unify (Term* a, Term* b);

            // semi-deterministic Prolog system predicates

            bool pl_string_1 (Term* a);

            bool pl_var_1 (Term* a);

            void mark_children () override;

    };

    // TODO merge into Machine
    // Keep track of references for garbage collection.
    class World final : public GC_Object {

        private: // fields

            Interp_Impl::Objects objects;
            Array<Clause> clauses;

        public: // construction

            World ()
            : clauses(1024)
            {
                if (!objects.pin(this)) {
                    abort();
                }
            }

        public: // construction and garbage collection

            // -------------------- deprecated; use new_ instead
            Compound* new_compound_v (const char* name_, size_t arity, ...);
            Compound* new_compound (const char* name_, size_t arity);
            Compound* new_compound (String* name, size_t arity);
            Compound* new_compound (String* name, size_t arity, Term** args);
            Fiber* new_fiber ();
            template <typename T> T do_add_object (T object) {
                objects.add(object);
                return object;
            }
            // -------------------- end of deprecated members

            template <typename T, typename ...Arg> T* new_ (Arg&&... arg) {
                return objects.new_<T,Arg...>(std::forward<Arg>(arg)...);
            }

            bool pin (GC_Object* root) {
                return objects.pin(root);
            }

            bool unpin (GC_Object* root) {
                return objects.unpin(root);
            }

            void collect_garbage () {
                objects.collect();
            }

        private: // copy term for unification

            struct Parallel final {
                Term* a;
                Term* b;
            };

            Term*
            copy_term (Array<Parallel>& pars, Term* original) {
                Term* td = original->dereference();
                for (size_t i = 0; i < pars.count(); ++i) {
                    const Parallel& par = pars[i];
                    if (par.a == td) {
                        return par.b;
                    }
                }
                {
                    const Compound* c;
                    if (td->get_compound(&c)) {
                        size_t arity = c->arity;
                        Term** u_args = new Term*[arity];
                        for (size_t i = 0; i < arity; ++i) {
                            u_args[i] = copy_term(pars, c->args[i]);
                        }
                        Compound* u = new Compound(c->name->copy(), arity, u_args);
                        pars.add({td, u});
                        return u;
                    }
                }
                {
                    const String* s;
                    if (td->get_string(&s)) {
                        String* u = s->copy();
                        pars.add({td, u});
                        return u;
                    }
                }
                return td;
            }

        public: // database

            void
            assertz (const Clause& clause) {
                clauses.add(clause);
            }

        public: // unification

            Term*
            copy_term (Term* t) {
                Array<Parallel> pars (16);
                return copy_term(pars, t);
            }

            Frame* new_frame ();
            Frame* new_frame (size_t);

        // deprecated; use Fiber::unify instead
        public:
            bool unify (Term* a, Term* b, /*nullable*/ Frame** u);

            bool unify (Term* a, Term* b) {
                return unify(a, b, nullptr);
            }
    };

}

#endif
