class Integer;
class String;
class Compound;
class Var;

class Term : public Object {
    friend class World;
protected:
    // Destructor should only be accessed by Garbage_collection.
    virtual ~Term () {}
    virtual void do_print_debug (String* out) const = 0;
public:

    // type-checking and pattern-matching

        virtual bool is_var () const { return false; }
        virtual bool is_unbound () const { return false; }
        virtual bool get_compound (const Compound** val) const { return false; }
        virtual bool get_integer (intptr_t* val) const { return false; }
        virtual bool get_string (const String** val) const { return false; }
        virtual bool get_var (const Var** val) const { return false; }
        virtual bool get_var (Var** val) { return false; }

    // graph

        virtual const Term* dereference () const { return this; }
        virtual Term* dereference () { return this; }

    // debug

        void print_debug (String* out) const {
            const Term* t = dereference();
            t->do_print_debug(out);
        }
};
