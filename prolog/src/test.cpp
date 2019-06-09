#include "world.h"
#include "test.h"

void
run_tests () {
    World* w = new World();

    Test tests[] = {

        Test("call arg(-,+,-)", [w](){
            Fiber* fiber = w->new_fiber();
            Term* i = w->new_var();
            Term* term = w->new_compound_v("f", 3, w->new_string("a"), w->new_string("b"), w->new_string("c"));
            Term* arg = w->new_var();
            String* s = w->new_string();
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
            printf("sizeof(Object) = %zu\n", sizeof(Object));
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
            Term* a = w->new_compound_v("foo", 2, w->new_var(), w->new_var());
            Term* b = w->new_compound_v("foo", 2, w->new_var(), w->new_var());
            Term* c = w->new_compound_v("foo", 2, w->new_string("abc"), w->new_integer(123));
            Term* d = w->new_var();
            Term* e = w->new_var();
            Term* f = w->new_var();
            Term* g = w->new_integer(1);
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
            Term* a = w->new_var();
            Term* b = w->new_var();
            Term* c = w->new_integer(1);
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

        Test("garbage collection", [w](){
            Var* a = w->new_var();
            Var* b = w->new_var();
            Var* c = w->new_var();
            for (size_t i = 0; i < 10000000; ++i) {
                w->new_var();
            }
            w->unify(a, b);
            w->unify(b, c);
            w->add_root(a);
            w->collect_garbage();
            return true;
        }),

    };

    size_t num_tests = sizeof(tests) / sizeof(*tests);
    size_t num_fail = 0;
    for (size_t i = 0; i < num_tests; ++i) {
        Test& test = tests[i];
        printf("============================== [%zu/%zu] %s\n", i + 1, num_tests, test.name);
        bool ok = test.run();
        printf("------------------------------ [%zu/%zu] %s\n", i + 1, num_tests, ok ? "ok" : "fail");
        if (!ok) { ++num_fail; }
    }
    printf("%zu failed tests.\n", num_fail);
    Term* True = w->new_compound_v("true", 0);
    w->assertz({
        w->new_compound_v("p", 1, w->new_integer(1)),
        True
    });
    w->assertz({
        w->new_compound_v("p", 1, w->new_integer(2)),
        True
    });
}
