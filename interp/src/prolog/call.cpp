namespace Interp_Prolog {

    //  Each instance of this represents an active call to a Prolog predicate.
    class Call : public GC_Object {
        private:
            enum class State : char {
                RUN = 0
                , END = 1
            };
            State state;
            Fiber* fiber;

            World* world () {
                return fiber->world;
            }
        protected:
            Call (Fiber* fiber_)
            : state(State::RUN)
            , fiber(fiber_)
            {
                fiber_->push_frame();
            }

            virtual ~Call () {
                fiber->pop_frame();
            }

            virtual bool
            run () {
                return false;
            }

            void
            end () {
                state = State::END;
            }
        protected: // Fiber delegate
            bool
            unify (Term* a, Term* b) {
                return fiber->unify(a, b);
            }

            void
            undo () {
                fiber->pop_frame();
                fiber->push_frame();
            }
        protected: // World delegate
            Integer* new_integer (intptr_t i) {
                return world()->new_<Integer>(i);
            }
        public:
            bool has_next () {
                return state != State::END;
            }
            bool next () {
                undo();
                return run();
            }
            void mark_children () override {
                fiber->mark();
                GC_Object::mark_children();
            }
    };

    class Call_arg_3 final : public Call {
        private:
            Term* i;
            Term* term;
            Term* arg;
            size_t next_index;
        public:
            Call_arg_3 (Fiber* fiber, Term* i_, Term* term_, Term* arg_)
            : Call(fiber)
            , i(i_)
            , term(term_)
            , arg(arg_)
            , next_index(0) {
                assert(fiber != nullptr);
                assert(i_ != nullptr);
                assert(term_ != nullptr);
                assert(arg_ != nullptr);
            }
            bool run () override {
                const Compound* c;
                if (!term->dereference()->get_compound(&c)) {
                    return false;
                }
                if (next_index >= c->arity) {
                    end();
                    return false;
                }
                bool ok =
                    unify(i, new_integer(next_index+1))
                    && unify(arg, c->args[next_index]);
                ++next_index;
                return ok;
            }
            void mark_children () override {
                i->mark();
                term->mark();
                arg->mark();
                Call::mark_children();
            }
    };
}
