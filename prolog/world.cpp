struct Clause final {
    Term* head;
    Term* body;
};

// Keep track of references for garbage collection.
class World final {
private:
    // fields

        Array<Clause> clauses;

    // copy term for unification

        struct Parallel final {
            Term* a;
            Term* b;
        };

        Term* copy_term (Array<Parallel>& pars, Term* original) {
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

public:
    World () : clauses(1024) {
    }

    // database

        void assertz (const Clause& clause) {
            clauses.add(clause);
        }

    // copy term for unification

        Term* copy_term (Term* t) {
            Array<Parallel> pars (16);
            return copy_term(pars, t);
        }

    // unification

        Unification* new_unification () {
            return new Unification(1024);
        }

    // term construction

        Integer* new_integer (intptr_t i) { return new Integer(i); }
        String* new_string (const char* s) { return String::copy_cstr(s); }
        Var* new_var () { return new Var(); }
        Compound* new_compound_v (const char* name_, size_t arity, ...) {
            String* name = String::copy_cstr(name_);
            Term** args = new Term*[arity];
            va_list ap;
            va_start(ap, arity);
            for (size_t i = 0; i < arity; ++i) {
                args[i] = va_arg(ap, Term*);
            }
            va_end(ap);
            return new_compound(name, arity, args);
        }
        Compound* new_compound (const char* name_, size_t arity) {
            String* name = String::copy_cstr(name_);
            Term** args = new Term*[arity];
            for (size_t i = 0; i < arity; ++i) {
                args[i] = new_var();
            }
            return new_compound(name, arity, args);
        }
        Compound* new_compound (String* name, size_t arity) {
            Term** args = new Term*[arity];
            for (size_t i = 0; i < arity; ++i) {
                args[i] = new_var();
            }
            return new_compound(name, arity, args);
        }
        Compound* new_compound (String* name, size_t arity, Term** args) {
            return new Compound(name, arity, args);
        }
};

void test_copy_term (World* world) {
    String s(1024);
    Term* a = world->new_var();
    Term* b = world->new_var();
    Term* c = world->new_integer(1);
    Term* d = world->new_compound_v("f", 1, a);
    Term* e = world->new_compound_v("g", 7, a, b, c, b, a, d, d);
    Term* f = world->copy_term(e);
    s.clear(); a->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); b->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); c->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); d->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); e->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); f->print_debug(&s); s.write_to(stdout); putchar('\n');
}
