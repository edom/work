// undoable unification

struct Unification final {
    struct Binding final {
        Var* var;
        Term* ori;
    };
    size_t limit;
    size_t count;
    Binding* bindings;
    Unification (size_t limit) {
        this->limit = limit;
        this->count = 0;
        this->bindings = new Binding[limit];
    }
    ~Unification () {
        delete[] bindings;
        bindings = nullptr;
    }
    void save_once (Var* var) {
        size_t i = 0;
        while (i < count) {
            Binding& b = bindings[i];
            if (b.var == var) {
                // Already saved. Don't overwrite the saved value.
                return;
            }
            ++i;
        }
        if (i >= limit) {
            printf("Sorry, I cannot keep track of more than %zu bindings.\n", limit);
            abort();
        }
        {
            Binding& b = bindings[i];
            b.var = var;
            b.ori = var->v;
        }
        count = i + 1;
    }
    void restore () {
        while (count > 0) {
            Binding& b = bindings[count - 1];
            b.var->v = b.ori;
            --count;
        }
    }
    void set (Var* var, Term* val) {
        save_once(var);
        var->v = val;
    }
    bool unify (Term* a, Term* b) {
        if (a == b) { return true; }
        //  Preconditions: a != b
        Var* a_var = nullptr;
        Var* b_var = nullptr;
        bool a_is_var = a->get_var(&a_var);
        bool b_is_var = b->get_var(&b_var);
        Term* ar = a_var == nullptr ? nullptr : a_var->v;
        Term* br = a_var == nullptr ? nullptr : b_var->v;
        bool ar_nul = ar == nullptr;
        bool br_nul = br == nullptr;
        //  This variable-variable unification is a quadratic time linked-list insertion sort?
        //  We want to maintain this ordering:
        //      IF a_is_var AND b_is_var THEN a <= b
        //      IF a_is_var AND ar_var THEN a < ar
        //      IF b_is_var AND br_var THEN b < br
        //      IF a_is_var AND ar_var AND b_is_var THEN a < ar <= b
        if (a_is_var && b_is_var) {
            if (a > b) { return unify(b, a); }
            //  Preconditions:
            //      a < b
            //      a < ar
            //      b < br
            if (ar_nul && br_nul) {
                //  Postconditions:
                //      ar' = b
                //      a < ar' <= b
                set(a_var, b);
                return true;
            }
            if (ar_nul) {
                set(a_var, b);
                return true;
            }
            if (br_nul) {
                if (ar == b) { return true; }
                if (ar < b) { return unify(ar, b); }
                // ar > b
                set(a_var, b);
                return unify(b, ar);
            }
            return unify(ar, br);
        }
        if (a_is_var) {
            if (ar_nul) {
                set(a_var, b);
                return true;
            }
            return unify(ar, b);
        }
        if (b_is_var) {
            return unify(b, a);
        }
        {
            intptr_t x;
            intptr_t y;
            if (a->get_integer(&x)) {
                if (b->get_integer(&y)) {
                    return x == y;
                }
                return false;
            }
        }
        {
            const String* x;
            const String* y;
            if (a->get_string(&x)) {
                if (b->get_string(&y)) {
                    return x->equals(y);
                }
                return false;
            }
        }
        {
            const Compound* x;
            const Compound* y;
            if (a->get_compound(&x)) {
                if (b->get_compound(&y)) {
                    if (x->arity != y->arity) { return false; }
                    if (!x->name->equals(y->name)) { return false; }
                    for (size_t i = 0; i < x->arity; ++i) {
                        if (!unify(x->args[i], y->args[i])) {
                            return false;
                        }
                    }
                    return true;
                }
                return false;
            }
        }
        return false;
    }
};
