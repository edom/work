struct Binding final {
    Var* var;
    Term* ori;
};

// undoable unification
class Frame final : public Object, private Array<Binding> {
    friend class World;
    public:
        Frame (size_t limit)
        : Array(limit) {
        }
        void save_once (Var* var) {
            size_t i = 0;
            while (i < count_) {
                Binding& b = at(i);
                if (b.var == var) {
                    // Already saved. Don't overwrite the saved value.
                    return;
                }
                ++i;
            }
            {
                Binding& b = at(i);
                b.var = var;
                b.ori = var->v;
            }
            count_ = i + 1;
        }
        void set (Var* var, Term* val) {
            save_once(var);
            var->v = val;
        }
    public:
        void restore_to (size_t sp) {
            while (count_ > sp) {
                Binding& b = at(count_ - 1);
                b.var->v = b.ori;
                --count_;
            }
        }
        // stack pointer
        size_t get_sp () const {
            return count_;
        }
        void restore () {
            return restore_to(0);
        }
        bool unify (Term* a, Term* b) {
            if (a == b) { return true; }
            //  Preconditions: a != b
            Var* a_var = nullptr;
            Var* b_var = nullptr;
            bool a_is_var = a->get_var(&a_var);
            bool b_is_var = b->get_var(&b_var);
            Term* ar = a_var == nullptr ? nullptr : a_var->v;
            Term* br = b_var == nullptr ? nullptr : b_var->v;
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
