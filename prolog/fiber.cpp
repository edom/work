// Prolog "thread" (not OS thread)
class Fiber : public Object {
    friend class Call;
    friend class World;

    private:
        World* world;
        Frame frame;
        Array<size_t> sp_stack;

    private:

        Fiber (World* world_)
        : world(world_)
        , frame(1024)
        , sp_stack(1024)
        {
        }

        void push_frame () {
            sp_stack.push(frame.get_sp());
        }

        void pop_frame () {
            size_t sp = sp_stack.pop();
            frame.restore_to(sp);
        }

        bool unify (Term* a, Term* b) {
            return frame.unify(a, b);
        }

    public: // semi-deterministic Prolog system predicates

        bool pl_string_1 (Term* a) {
            const String* z;
            return a->dereference()->get_string(&z);
        }

        bool pl_var_1 (Term* a) {
            return a->dereference()->is_var();
        }

    protected:
        void mark_children () override {
            frame.mark();
            Object::mark_children();
        }
};
