/*
Problems:
    - unification not yet implemented
    - garbage collection not implemented
*/
#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

// mutable byte string
struct String {
    size_t limit;
    size_t count;
    char* bytes;
    String (size_t limit) {
        this->limit = limit;
        this->count = 0;
        this->bytes = new char[limit];
    }
    ~String () {
        limit = 0;
        count = 0;
        delete[] bytes;
        bytes = nullptr;
    }
    static String* copy_cstr (const char* cstr) {
        size_t limit = strlen(cstr);
        String* s = new String(limit);
        s->append_trunc(cstr);
        return s;
    }
    void clear () {
        count = 0;
    }
    bool equals (const String* that) const {
        assert(this->count <= this->limit);
        assert(that->count <= that->limit);
        if (this->count != that->count) {
            return false;
        }
        size_t i = 0;
        while (i < count) {
            if (this->bytes[i] != that->bytes[i]) {
                return false;
            }
            ++i;
        }
        return true;
    }
    size_t append_trunc (const String* that) {
        size_t i = 0;
        while (count < limit && i < that->count) {
            bytes[count] = that->bytes[i];
            ++count;
            ++i;
        }
        return i;
    }
    size_t append_trunc (const char* cstr) {
        size_t i = 0;
        while (count < limit && cstr[i] != 0) {
            bytes[count] = cstr[i];
            ++count;
            ++i;
        }
        return i;
    }
    void printf (const char* format, ...) {
        va_list ap;
        va_start(ap, format);
        size_t n = vsnprintf(bytes + count, limit - count, format, ap);
        count += n;
        if (count > limit) {
            puts("String.printf: count > limit");
            abort();
        }
        va_end(ap);
    }
    size_t write_to (FILE* out) const {
        return fwrite(bytes, 1, count, out);
    }
};

enum Type {
    INTEGER
    , STRING
    , COMPOUND
    , VAR
};

// TODO garbage-collect terms

class Term;

struct Compound {
    String* name;
    size_t arity;
    Term** args;
};

class Term {
private:
    Type type;
    union {
        Compound c;
        intptr_t i;
        String* s;
        Term* v;
    };
    Term () {
        this->type = VAR;
        this->v = nullptr;
    }
public:
    ~Term () {
    }
    static Term* new_integer (intptr_t i) {
        Term* t = new Term();
        t->type = INTEGER;
        t->i = i;
        return t;
    }
    static Term* new_string (const char* s) {
        Term* t = new Term();
        t->type = STRING;
        t->s = String::copy_cstr(s);
        return t;
    }
    static Term* new_compound_v (const char* name_, size_t arity, ...) {
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
    static Term* new_compound (const char* name_, size_t arity) {
        String* name = String::copy_cstr(name_);
        Term** args = new Term*[arity];
        for (size_t i = 0; i < arity; ++i) {
            args[i] = new_var();
        }
        return new_compound(name, arity, args);
    }
    static Term* new_compound (String* name, size_t arity) {
        Term** args = new Term*[arity];
        for (size_t i = 0; i < arity; ++i) {
            args[i] = new_var();
        }
        return new_compound(name, arity, args);
    }
    static Term* new_compound (String* name, size_t arity, Term** args) {
        Term* t = new Term;
        t->type = COMPOUND;
        t->c.name = name;
        t->c.arity = arity;
        t->c.args = args;
        return t;
    }
    static Term* new_var () {
        return new Term;
    }
    bool is_var () const {
        return type == VAR;
    }
    bool is_unbound () const {
        const Term* t = dereference();
        return t->type == VAR && t->v == nullptr;
    }
    bool get_compound (const Compound** val) const {
        if (type != COMPOUND) { return false; }
        *val = &c;
        return true;
    }
    bool get_integer (intptr_t* val) const {
        if (type != INTEGER) { return false; }
        *val = i;
        return true;
    }
    bool get_string (const String** val) const {
        if (type != STRING) { return false; }
        *val = s;
        return true;
    }
    const Term* dereference () const {
        if (type == VAR && v != nullptr) {
            return v->dereference();
        }
        return this;
    }
    Term* dereference () {
        if (type == VAR && v != nullptr) {
            return v->dereference();
        }
        return this;
    }
    // get direct/immediate/proximate referent, not ultimate/transitive
    Term* get_referent () {
        if (type == VAR) { return v; }
        return this;
    }
    void set_referent (Term* val) {
        if (type != VAR) { abort(); }
        v = val;
    }
    void print_debug (String* out) const {
        const Term* t = dereference();
        switch (t->type) {
            case INTEGER:
                out->printf("%" PRIdPTR, t->i);
                break;
            case STRING:
                out->append_trunc(t->s);
                break;
            case COMPOUND:
                {
                    out->append_trunc(t->c.name);
                    out->printf("(");
                    for (size_t i = 0; i < t->c.arity; ++i) {
                        t->c.args[i]->print_debug(out);
                        if (i+1 < t->c.arity) {
                            out->printf(",");
                        }
                    }
                    out->printf(")");
                }
                break;
            case VAR:
                out->printf("_%p", t);
                break;
            default:
                assert(false);
        }
    }
};

struct Binding {
    Term* var;
    Term* old;
};

struct Unification {
    size_t limit;
    size_t count;
    struct Binding* bindings;
    Unification (size_t limit) {
        this->limit = limit;
        this->count = 0;
        this->bindings = new Binding[limit];
    }
    ~Unification () {
        delete[] bindings;
        bindings = nullptr;
    }
    void set (Term* var, Term* val) {
        assert(var->is_unbound());
        if (count >= limit) {
            puts("unification too large");
            abort();
        }
        Binding& b = bindings[count];
        b.var = var;
        b.old = var->get_referent();
        var->set_referent(val);
        ++count;
    }
    void undo () {
        while (count > 0) {
            Binding& b = bindings[count-1];
            b.var->set_referent(b.old);
            --count;
        }
    }
    bool unify (Term* a, Term* b) {
        bool a_var = a->is_var();
        bool b_var = a->is_var();
        Term* ar = a->get_referent();
        Term* br = b->get_referent();
        bool ar_nul = ar == nullptr;
        bool br_nul = br == nullptr;
        //  This variable-variable unification is a quadratic time linked-list insertion sort?
        //  We want to maintain this ordering:
        //      IF a_var AND b_var THEN a <= b
        //      IF a_var AND ar_var THEN a < ar
        //      IF b_var AND br_var THEN b < br
        //      IF a_var AND ar_var AND b_var THEN a < ar <= b
        if (a_var && b_var) {
            if (a == b) { return true; }
            if (a > b) { return unify(b, a); }
            //  Preconditions:
            //      a < b
            //      a < ar
            //      b < br
            if (ar_nul && br_nul) {
                //  Postconditions:
                //      ar' = b
                //      a < ar' <= b
                set(a, b);
                return true;
            }
            if (ar_nul) {
                set(a, b);
                return true;
            }
            if (br_nul) {
                if (ar == b) { return true; }
                if (ar < b) { return unify(ar, b); }
                // ar > b
                set(a, b);
                return unify(b, ar);
            }
            return unify(ar, br);
        }
        if (a_var) {
            if (ar_nul) {
                set(a, b);
                return true;
            }
            return unify(ar, b);
        }
        if (b_var) {
            return unify(b, a);
        }
        {
            intptr_t x;
            intptr_t y;
            if (a->get_integer(&x) && b->get_integer(&y)) {
                return x == y;
            }
        }
        {
            const String* x;
            const String* y;
            if (a->get_string(&x) && b->get_string(&y)) {
                return x->equals(y);
            }
        }
        {
            const Compound* x;
            const Compound* y;
            if (a->get_compound(&x) && b->get_compound(&y)) {
                if (x->arity != y->arity) { return false; }
                if (!x->name->equals(y->name)) { return false; }
                for (size_t i = 0; i < x->arity; ++i) {
                    if (!unify(x->args[i], y->args[i])) {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }
};

int
main (int argc, char* argv[]) {
    Unification* u = new Unification(1024);
    String* s = new String(1024);
    //Term* a = Term::new_integer(1);
    Term* a = Term::new_compound_v("foo", 2, Term::new_var(), Term::new_var());
    Term* b = Term::new_compound_v("foo", 2, Term::new_var(), Term::new_var());
    Term* c = Term::new_compound_v("foo", 2, Term::new_string("abc"), Term::new_integer(123));
    Term* d = Term::new_var();
    Term* e = Term::new_var();
    Term* f = Term::new_var();
    Term* g = Term::new_integer(1);
    puts("------------------------------ before unify");
    s->clear(); a->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); b->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); c->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); d->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); e->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); f->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); g->print_debug(s); s->write_to(stdout); putchar('\n');
    // ------------------------------ unify
    bool success =
        // u->unify(a, b) &&
        u->unify(g, d) &&
        // FIXME Why does unify(d,g) work but unify(g,d) doesn't?
        // u->unify(b, c) &&
        true;
    printf("------------------------------ after unify (success = %d)\n", success);
    s->clear(); a->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); b->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); c->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); d->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); e->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); f->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); g->print_debug(s); s->write_to(stdout); putchar('\n');
    u->undo();
    puts("------------------------------ after undo");
    s->clear(); a->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); b->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); c->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); d->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); e->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); f->print_debug(s); s->write_to(stdout); putchar('\n');
    s->clear(); g->print_debug(s); s->write_to(stdout); putchar('\n');
    return EXIT_SUCCESS;
}

