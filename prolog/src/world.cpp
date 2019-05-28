Var*
World::new_var () {
    return do_add_object(new Var());
}

String*
World::new_string () {
    return do_add_object(new String(128));
}

String*
World::new_string (const char* s) {
    return do_add_object(String::copy_cstr(s));
}

Integer*
World::new_integer (intptr_t value) {
    return do_add_object(new Integer(value));
}

Compound*
World::new_compound_v (const char* name_, size_t arity, ...) {
    String* name = new_string(name_);
    Term** args = new Term*[arity];
    va_list ap;
    va_start(ap, arity);
    for (size_t i = 0; i < arity; ++i) {
        args[i] = va_arg(ap, Term*);
    }
    va_end(ap);
    return new_compound(name, arity, args);
}

Compound*
World::new_compound (const char* name_, size_t arity) {
    String* name = String::copy_cstr(name_);
    Term** args = new Term*[arity];
    for (size_t i = 0; i < arity; ++i) {
        args[i] = new_var();
    }
    return new_compound(name, arity, args);
}

Compound*
World::new_compound (String* name, size_t arity) {
    Term** args = new Term*[arity];
    for (size_t i = 0; i < arity; ++i) {
        args[i] = new_var();
    }
    return new_compound(name, arity, args);
}

Compound*
World::new_compound (String* name, size_t arity, Term** args) {
    return do_add_object(new Compound(name, arity, args));
}

Fiber*
World::new_fiber () {
    return do_add_object(new Fiber(this));
}

Frame*
World::new_frame () {
    return do_add_object(new Frame(16));
}


Frame*
World::new_frame (size_t limit) {
    return do_add_object(new Frame(limit));
}

bool
World::unify (Term* a, Term* b, Frame** u_) {
    Frame* u = new_frame();
    if (u_ != nullptr) { *u_ = u; }
    return u->unify(a, b);
}
