// TODO Replace Term Type switch with Term1 polymorphism

enum class Type : char {
    INTEGER = 1
    , STRING = 2
    , COMPOUND = 3
    , VAR = 4
};

class Term;

struct Compound final {
    String* name;
    size_t arity;
    Term** args;
};

class Term : public Object {
    friend class World;

    // fields

        Type type;
        union {
            Compound* c;
            intptr_t i;
            String* s;
            Term* v;
        };

    Term () {
        this->type = Type::VAR;
        this->v = nullptr;
    }

    Refs get_gc_out_refs () override {
        switch (type) {
            case Type::VAR: {
                return Refs(this, 1, [] (Object* self, size_t i) -> Object* {
                    switch (i) {
                        case 0: return ((Term*)self)->v;
                        default: return nullptr;
                    }
                });
            }
            default: {
                return REFS_NONE(this);
            }
        }
    }
public:
    ~Term () {
    }

    // type-checking and pattern-matching

        bool is_var () const {
            return type == Type::VAR;
        }
        bool is_unbound () const {
            const Term* t = dereference();
            return t->type == Type::VAR && t->v == nullptr;
        }
        bool get_compound (const Compound** val) const {
            if (type != Type::COMPOUND) { return false; }
            *val = c;
            return true;
        }
        bool get_integer (intptr_t* val) const {
            if (type != Type::INTEGER) { return false; }
            *val = i;
            return true;
        }
        bool get_string (const String** val) const {
            if (type != Type::STRING) { return false; }
            *val = s;
            return true;
        }

    // graph

        const Term* dereference () const {
            if (type == Type::VAR && v != nullptr) {
                return v->dereference();
            }
            return this;
        }
        Term* dereference () {
            if (type == Type::VAR && v != nullptr) {
                return v->dereference();
            }
            return this;
        }
        // get direct/immediate/proximate referent, not ultimate/transitive
        Term* get_referent () {
            assert(type == Type::VAR);
            return v;
        }
        void set_referent (Term* val) {
            assert(type == Type::VAR);
            v = val;
        }

    // debug

        void print_debug (String* out) const {
            const Term* t = dereference();
            switch (t->type) {
                case Type::INTEGER:
                    out->printf("%" PRIdPTR, t->i);
                    break;
                case Type::STRING:
                    out->append_trunc(t->s);
                    break;
                case Type::COMPOUND:
                    {
                        out->append_trunc(t->c->name);
                        out->printf("(");
                        for (size_t i = 0; i < t->c->arity; ++i) {
                            t->c->args[i]->print_debug(out);
                            if (i+1 < t->c->arity) {
                                out->printf(",");
                            }
                        }
                        out->printf(")");
                    }
                    break;
                case Type::VAR:
                    out->printf("_%p", t);
                    break;
                default:
                    assert(false);
            }
        }
};

class Term1 : public Object {
public:
    virtual ~Term1 () = 0;
    virtual bool is_var () { return false; }
};
Term1::~Term1 () {}

class Var : public Term1 {
    Term* v;
public:
    virtual ~Var () {}
    bool is_var () override { return true; }
};
