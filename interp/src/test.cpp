#include "pch.h"

#include "std.h"
#include "test.h"

#include "prolog/world.h"

namespace Interp_Prolog {

    using Interp_Impl::Std_Vector;
    using Interp_Impl::Test;

    Std_Vector<Test>
    get_tests (World* w) {

        return {

            Test("call arg(-,+,-)", [w](){
                Fiber* fiber = w->new_fiber();
                Term* i = w->new_<Var>();
                Term* term = w->new_compound_v("f", 3, w->new_<String>("a"), w->new_<String>("b"), w->new_<String>("c"));
                Term* arg = w->new_<Var>();
                String* s = w->new_<String>();
                Call_arg_3 arg_3 (fiber, i, term, arg);
                while (arg_3.has_next()) {
                    if (!arg_3.next()) { continue; }
                    s->clear();
                    i->print_debug(s);
                    term->print_debug(s);
                    arg->print_debug(s);
                    s->write_to(stdout);
                    putchar('\n');
                }
                return true;
            }),

            Test("check sizeof", [](){
                class A1 { char a; };
                class A2 : A1 { char b; };
                class A3 : A2 { char c; };
                class B1 : A1 { int b; };
                class C1 { char b; char c; };
                class D1 : A1, C1 { };
                printf("sizeof(A1) = %zu\n", sizeof(A1));
                printf("sizeof(A2) = %zu\n", sizeof(A2));
                printf("sizeof(A3) = %zu\n", sizeof(A3));
                printf("sizeof(B1) = %zu\n", sizeof(B1));
                printf("sizeof(C1) = %zu\n", sizeof(C1));
                printf("sizeof(D1) = %zu\n", sizeof(D1));
                printf("sizeof(GC_Object) = %zu\n", sizeof(GC_Object));
                printf("sizeof(Foreign_object<int>) = %zu\n", sizeof(Foreign_object<int>));
                printf("sizeof(Term) = %zu\n", sizeof(Term));
                printf("sizeof(Var) = %zu\n", sizeof(Var));
                printf("sizeof(String) = %zu\n", sizeof(String));
                return true;
            }),

            Test("unification", [w](){
                Frame* u = w->new_frame();
                String s_(1024);
                String* s = &s_;
                Term* a = w->new_compound_v("foo", 2, w->new_<Var>(), w->new_<Var>());
                Term* b = w->new_compound_v("foo", 2, w->new_<Var>(), w->new_<Var>());
                Term* c = w->new_compound_v("foo", 2, w->new_<String>("abc"), w->new_<Integer>(123));
                Term* d = w->new_<Var>();
                Term* e = w->new_<Var>();
                Term* f = w->new_<Var>();
                Term* g = w->new_<Integer>(1);
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
                    u->unify(a, b) &&
                    u->unify(a, c) &&
                    u->unify(d, f) &&
                    u->unify(e, f) &&
                    u->unify(d, g) &&
                    true;
                printf("------------------------------ after unify (success = %d)\n", success);
                s->clear(); a->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); b->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); c->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); d->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); e->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); f->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); g->print_debug(s); s->write_to(stdout); putchar('\n');
                u->restore();
                puts("------------------------------ after restore");
                s->clear(); a->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); b->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); c->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); d->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); e->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); f->print_debug(s); s->write_to(stdout); putchar('\n');
                s->clear(); g->print_debug(s); s->write_to(stdout); putchar('\n');
                return true;
            }),

            Test("copy_term", [w](){
                String s(1024);
                Term* a = w->new_<Var>();
                Term* b = w->new_<Var>();
                Term* c = w->new_<Integer>(1);
                Term* d = w->new_compound_v("f", 1, a);
                Term* e = w->new_compound_v("g", 7, a, b, c, b, a, d, d);
                Term* f = w->copy_term(e);
                s.clear(); a->print_debug(&s); s.write_to(stdout); putchar('\n');
                s.clear(); b->print_debug(&s); s.write_to(stdout); putchar('\n');
                s.clear(); c->print_debug(&s); s.write_to(stdout); putchar('\n');
                s.clear(); d->print_debug(&s); s.write_to(stdout); putchar('\n');
                s.clear(); e->print_debug(&s); s.write_to(stdout); putchar('\n');
                s.clear(); f->print_debug(&s); s.write_to(stdout); putchar('\n');
                return true;
            }),

//            Test("garbage collection", [w](){
//                Var* a = w->new_<Var>();
//                Var* b = w->new_<Var>();
//                Var* c = w->new_<Var>();
//                for (size_t i = 0; i < 1000000; ++i) {
//                    w->new_<Var>();
//                }
//                w->unify(a, b);
//                w->unify(b, c);
//                w->add_root(a);
//                w->collect_garbage();
//                return true;
//            }),

        };

    }

    void
    run_tests () {

        World* w = new World();

        Std_Vector<Test> tests = get_tests(w);

        run(tests);

        Term* True = w->new_compound_v("true", 0);
        w->assertz({
            w->new_compound_v("p", 1, w->new_<Integer>(1)),
            True
        });
        w->assertz({
            w->new_compound_v("p", 1, w->new_<Integer>(2)),
            True
        });

    }

}

namespace Interp_Impl {

    void
    run (const Std_Vector<Test>& tests) {
        size_t total = tests.size();
        size_t fail = 0;
        size_t current = 0;
        for (auto& test : tests) {
            ++current;
            printf("============================== [%zu/%zu] %s\n", current, total, test.name);
            bool ok = test.run();
            printf("------------------------------ [%zu/%zu] %s\n", current, total, ok ? "ok" : "fail");
            if (!ok) { ++fail; }
        }
        std::cout << fail << " failed tests.\n";
    }

    Std_Vector<Test>
    get_tests () {

        return {

            Test("garbage collection again", [] () {
                Machine m;
                m.load_standard_library();
                Value* plus  = m.lookup("+");
                Value* collect_garbage = m.lookup("collect_garbage");
                Value* show_gc_stats = m.lookup("show_gc_stats");
                m.push(m.new_<Integer>(1));
                m.push(m.new_<Integer>(2));
                m.push(m.new_<Integer>(3));
                collect_garbage->mutate(m);
                collect_garbage->mutate(m);
                plus->mutate(m);
//                collect_garbage->mutate(m);
//                plus->mutate(m);
                return true;
            }),

        };

    }

    void run_tests () {
        Interp_Prolog::run_tests();
        Std_Vector<Test> tests = get_tests();
        run(tests);
    }

}
