class Var final : public Term {
    friend class Frame;
    friend Var* World::new_var ();
    private:
        Term* v;
        Var () : v(nullptr) {}
        Var (Term* v_) : v(v_) {}
    public:
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
    protected:
        void mark_children () override {
            if (v != nullptr) { v->mark(); }
            Term::mark_children();
        }
        void do_print_debug (String* out) const override {
            out->printf("_%p", this);
        }
};
