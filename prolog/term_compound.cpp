struct Compound final : public Term {
    public:
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
        void mark_children () override {
            name->mark();
            Term::mark_children();
            // TODO make args an Array, and make Array an Object
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
