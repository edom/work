/*
Problems:
    - unification not yet implemented
    - garbage collection not implemented
*/

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "array.cpp"
#include "string.cpp"
#include "object.cpp"
#include "term.cpp"
#include "unify.cpp"
#include "world.cpp"

void
test_unification (World* world) {
    Unification* u = new Unification(1024);
    String s_(1024);
    String* s = &s_;
    Term* a = world->new_compound_v("foo", 2, world->new_var(), world->new_var());
    Term* b = world->new_compound_v("foo", 2, world->new_var(), world->new_var());
    Term* c = world->new_compound_v("foo", 2, world->new_string("abc"), world->new_integer(123));
    Term* d = world->new_var();
    Term* e = world->new_var();
    Term* f = world->new_var();
    Term* g = world->new_integer(1);
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
}

void test_size () {
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
    printf("sizeof(Term1) = %zu\n", sizeof(Term1));
    printf("sizeof(Var) = %zu\n", sizeof(Var));
    printf("sizeof(String) = %zu\n", sizeof(String));
}

void test_gc (World& w) {
    Array<Object*> objects (1024);
    Array<Object*> roots (1024);
    Term* a = w.new_var();
    Term* b = w.new_var();
    Term* c = w.new_var();
    Term* d = w.new_var();
    a->set_referent(b);
    b->set_referent(c);
    objects.add(a);
    objects.add(b);
    objects.add(c);
    objects.add(d);
    roots.add(a);
    Garbage_collection gc;
    gc.set_verbosity(Garbage_collection::VERBOSITY_TRACE);
    gc.delete_objects_unreachable_from(objects, roots);
}

int
main (int argc, char* argv[]) {
    World w;
    Term* True = w.new_compound_v("true", 0);
    w.assertz({
        head: w.new_compound_v("p", 1, w.new_integer(1)),
        body: True
    });
    w.assertz({
        head: w.new_compound_v("p", 1, w.new_integer(2)),
        body: True
    });
    test_size();
    test_gc(w);
    // test_copy_term(&w);
    // test_unification(&w);
    return EXIT_SUCCESS;
}
