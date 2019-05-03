// immutable integer
struct Integer final : public Term {
    const intptr_t i;
    Integer (intptr_t i_) : i(i_) {}
    bool get_integer (intptr_t* val) const override {
        *val = i;
        return true;
    }
protected:
    Refs get_gc_out_refs () { return REFS_NONE(this); }
    void do_print_debug (String* out) const override {
        out->printf("%" PRIdPTR, i);
    }
};

struct Compound final : public Term {
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

protected:
    Refs get_gc_out_refs () override {
        // TODO
        return Refs(this, 1, [] (Object* self, size_t _i) -> Object* {
            return static_cast<Compound*>(self)->name;
        });
    }
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
    friend class Unification;
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
    void set_referent (Term* v) { this->v = v; }
protected:
    Refs get_gc_out_refs () override {
        return Refs(this, 1, [] (Object* self, size_t _i) -> Object* {
            return static_cast<Var*>(self)->v;
        });
    }
    void do_print_debug (String* out) const override {
        out->printf("_%p", this);
    }
};
