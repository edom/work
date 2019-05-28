#ifndef PROLOG_WORLD_H_INCLUDED
#define PROLOG_WORLD_H_INCLUDED

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

class Integer;
class Var;
class Frame;
class Fiber;

// Keep track of references for garbage collection.
class World final {

    private: // fields

        Array<Object*> objects;
        Array<Clause> clauses;
        Array<Object*> roots;

        template <typename T>
        T* do_add_object (T* x) {
            objects.add(x);
            return x;
        }

    public: // construction

        World ()
        : objects(1048576)
        , clauses(1024)
        , roots(64) {
        }

    public: // construction and garbage collection

        Var* new_var ();
        String* new_string ();
        String* new_string (const char* s);
        Integer* new_integer (intptr_t value);
        Compound* new_compound_v (const char* name_, size_t arity, ...);
        Compound* new_compound (const char* name_, size_t arity);
        Compound* new_compound (String* name, size_t arity);
        Compound* new_compound (String* name, size_t arity, Term** args);
        Fiber* new_fiber ();

        bool add_root (Object* root) {
            roots.add(root);
            return true;
        }

        bool remove_root (Object* root) {
            size_t n = roots.count();
            for (size_t i = 0; i < n; ++i) {
                if (roots[i] == root) {
                    roots[i] = nullptr;
                    return true;
                }
            }
            return false;
        }

        //  Manually called.
        //  Single-threaded.
        void collect_garbage () {
            Garbage_collection gc;
            gc.set_verbosity(Garbage_collection::VERBOSITY_TRACE);
            gc.delete_objects_unreachable_from(objects, roots);
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
                    pars.add({a:td, b:u});
                    return u;
                }
            }
            {
                const String* s;
                if (td->get_string(&s)) {
                    String* u = s->copy();
                    pars.add({a:td, b:u});
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

#endif
