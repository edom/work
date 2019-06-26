#include <cstring>

#include "term.h"

#include "../array.h"

namespace Interp_Prolog {

    void
    Term::print_debug (String* out) const {
        const Term* t = dereference();
        t->do_print_debug(out);
    }

    bool
    Term::is_Var () const {
        return false;
    }

    bool
    Term::is_unbound () const {
        return false;
    }

    bool
    Term::get_compound (const Compound** val) const {
        (void) (val);
        return false;
    }

    bool
    Term::get_integer (intptr_t* val) const {
        (void) (val);
        return false;
    }

    bool
    Term::get_string (const String** val) const {
        (void) (val);
        return false;
    }

    bool
    Term::get_var (Var** val) {
        (void) (val);
        return false;
    }

    bool
    Term::get_var (const Var** val) const {
        (void) (val);
        return false;
    }

    Var*
    Term::as_Var_or_null () {
        return nullptr;
    }

    const Var*
    Term::as_Var_or_null () const {
        return nullptr;
    }

    Term*
    Term::dereference () {
        return this;
    }

    const Term*
    Term::dereference () const {
        return this;
    }

    Var::Var ()
    : v(nullptr)
    {
    }

    Var::Var (Term* v_)
    : v(v_)
    {
    }

    Var::~Var () {
        v = nullptr;
    }

    bool
    Var::is_Var () const {
        return true;
    }

    bool
    Var::is_unbound () const {
        const Term* t = dereference();
        return t->as_Var_or_null() == nullptr;
    }

    bool
    Var::get_var (const Var** v) const {
        *v = this;
        return true;
    }

    bool
    Var::get_var (Var** v) {
        *v = this;
        return true;
    }

    Var*
    Var::as_Var_or_null () {
        return this;
    }

    const Var*
    Var::as_Var_or_null () const {
        return this;
    }

    const
    Term* Var::dereference () const {
        if (v == nullptr) {
            return this;
        }
        return v->dereference();
    }

    Term*
    Var::dereference () {
        if (v == nullptr) {
            return this;
        }
        return v->dereference();
    }

    void
    Var::mark_children () {
        if (v != nullptr) {
            v->mark();
        }
        Term::mark_children();
    }

    void
    Var::do_print_debug (String* out) const {
        out->printf("_%p", this);
    }

    String::String ()
    : Array(128) {
    }

    String::String (size_t limit)
    : Array(limit) {
    }

    String::String (const char* s)
    : Array(strlen(s)) {
        // inlined from copy_cstr
        append_trunc(s);
    }

}
