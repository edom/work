struct Clause final {
    Term* head;
    Term* body;
};

// Keep track of references for garbage collection.
class World final {
private:
    // fields

        Array<Clause> clauses;

    // copy term

        struct Parallel final {
            Term* a;
            Term* b;
        };

        Term* copy_term_shallow (Array<Parallel>& pars, Term* original) {
            Term* td = original->dereference();
            for (size_t i = 0; i < pars.count(); ++i) {
                const Parallel& par = pars[i];
                if (par.a == td) {
                    return par.b;
                }
            }
            switch (td->type) {
                case Type::INTEGER:
                case Type::STRING: {
                    // Can we assume that the string will not be mutated?
                    Term* u = new Term();
                    *u = *td;
                    pars.add({a:td, b:u});
                    return u;
                }
                case Type::VAR: {
                    Term* u = new Term();
                    pars.add({a:td, b:u});
                    return u;
                }
                case Type::COMPOUND: {
                    Term* u = new Term();
                    size_t arity = td->c->arity;
                    u->type = Type::COMPOUND;
                    u->c->name = td->c->name;
                    u->c->arity = arity;
                    u->c->args = new Term*[arity];
                    for (size_t i = 0; i < arity; ++i) {
                        u->c->args[i] = copy_term_shallow(pars, td->c->args[i]);
                    }
                    pars.add({a:td, b:u});
                    return u;
                }
                default:
                    abort();
            }
        }

public:
    World () : clauses(1024) {
    }

    // database

        void assertz (const Clause& clause) {
            clauses.add(clause);
        }

    // copy term

        Term* copy_term_shallow (Term* t) {
            Array<Parallel> pars (16);
            return copy_term_shallow(pars, t);
        }

    // unification

        Unification* new_unification () {
            return new Unification(1024);
        }

    // term construction

        Term* new_integer (intptr_t i) {
            Term* t = new Term();
            t->type = Type::INTEGER;
            t->i = i;
            return t;
        }
        Term* new_string (const char* s) {
            Term* t = new Term();
            t->type = Type::STRING;
            t->s = String::copy_cstr(s);
            return t;
        }
        Term* new_compound_v (const char* name_, size_t arity, ...) {
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
        Term* new_compound (const char* name_, size_t arity) {
            String* name = String::copy_cstr(name_);
            Term** args = new Term*[arity];
            for (size_t i = 0; i < arity; ++i) {
                args[i] = new_var();
            }
            return new_compound(name, arity, args);
        }
        Term* new_compound (String* name, size_t arity) {
            Term** args = new Term*[arity];
            for (size_t i = 0; i < arity; ++i) {
                args[i] = new_var();
            }
            return new_compound(name, arity, args);
        }
        Term* new_compound (String* name, size_t arity, Term** args) {
            Term* t = new Term;
            t->type = Type::COMPOUND;
            t->c = new Compound();
            t->c->name = name;
            t->c->arity = arity;
            t->c->args = args;
            return t;
        }
        Term* new_var () {
            return new Term;
        }
};

void test_copy_term (World* world) {
    String s(1024);
    Term* a = world->new_var();
    Term* b = world->new_var();
    Term* c = world->new_integer(1);
    Term* d = world->new_compound_v("f", 1, a);
    Term* e = world->new_compound_v("g", 7, a, b, c, b, a, d, d);
    Term* f = world->copy_term_shallow(e);
    s.clear(); a->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); b->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); c->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); d->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); e->print_debug(&s); s.write_to(stdout); putchar('\n');
    s.clear(); f->print_debug(&s); s.write_to(stdout); putchar('\n');
}
