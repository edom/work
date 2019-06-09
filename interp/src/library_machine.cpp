#include "machine.h"
#include "std.h"
#include "value.h"

namespace Interp_Impl
{

    Machine::Machine () {
        //  The machine must be pinned but not adopted.
        objects.pin(this);
    }

    Machine::~Machine () {
        //  Delete all adoptees.
        objects.unpin(this);
    }

    void
    Machine::bind (const char* name, Value* v) {
        bind(Key(name), v);
    }

    void
    Machine::bind (const Key& key, Value* value) {
        bindings[key] = value;
    }

    void
    Machine::bind (Key&& key, Value* value) {
        bindings[key] = value;
    }

    Value* Machine::lookup (const Key& key) const {
        auto i = bindings.find(key);
        if (i == bindings.end()) {
            return &Errors::key_not_found;
        }
        return i->second;
    }

    Value* Machine::lookup (const char* name) const { return lookup(Key{name}); }
    Value* Machine::lookup (const Std_String& name) const { return lookup(name.c_str()); }

    Std_Vector<Std_String>
    Machine::find_keys_with_prefix (const char* prefix) const {
        Std_Vector<Std_String> matches;
        size_t prefix_length = strlen(prefix);
        for (auto& pair : bindings) {
            const Key& key = pair.first;
            if (prefix_length > key.length) { continue; }
            if (memcmp(prefix, key.bytes, prefix_length) == 0) {
                matches.push_back(key.to_std_string());
            }
        }
        return matches;
    }

    void Machine::push (Value* a) { stack.push_back(a); }
    Value* Machine::pop () {
        if (stack.empty()) { return &Errors::stack_underflow; }
        Value* a = stack.back();
        stack.pop_back();
        return a;
    }
    int Machine::pop_int () {
        Value* v = pop();
        v->throw_if_error();
        int i;
        if (!v->get_int(i)) { Errors::type_error.throw_(); }
        return i;
    }

    void Machine::print_stack () const {
        for (auto i = stack.begin(); i != stack.end(); ++i) {
            std::cout << (*i)->to_std_string();
            if (i != stack.end() - 1) {
                std::cout << " ";
            }
        }
    }

    void Machine::collect_garbage () {
        objects.collect();
    }

    void Machine::mark_children () {
        if (error != nullptr) {
            error->mark();
        }
        for (auto v : stack) {
            v->mark();
        }
        for (auto& p : bindings) {
            p.second->mark();
        }
        GC_Object::mark_children();
    }

    void Machine::show_gc_stats () {
        objects.show_gc_stats();
    }

    Operation::Operation (Function* imp_) : imp(imp_) {
    }

    Type* Operation::type () const { return &Types::Operation; }

    void Operation::mutate (Machine& m) {
        (*imp)(m);
    }

    Micro operator- (const timespec& a, const timespec& b) {
        Micro ds = a.tv_sec - b.tv_sec;
        Micro dn = a.tv_nsec - b.tv_nsec;
        return (1000000000*ds + dn) / 1000;
    }

    // -------------------- marshalling

    // default marshalling
    Value*      wrap (Machine& m, Value* a) { (void) m; return a; }
    Integer*    wrap (Machine& m, int a) { return m.new_<Integer>(a); }
    String*     wrap (Machine& m, const char* a) { return m.new_<String>(a); }

    // default unmarshalling
    template <typename R>
    R unwrap (Value*);

    template <> Integer*    unwrap (Value* a) { return a->be_integer_or_throw(); }
    template <> int         unwrap (Value* a) { return a->be_integer_or_throw()->get_int_or_throw(); }

    // marshal, call, and unmarshal
    template <class R, class...A>
    void call_native (Machine& m, std::function<R(A...)> f);

    template <class R>
    void call_native (Machine& m, std::function<R()> f) {
        R ret = f();
        Value* w_ret = wrap(m, ret);
        m.push(w_ret);
    }

    template <class R, class A, class...B>
    void call_native (Machine& m, std::function<R(A,B...)> f) {
        Value* w_arg = m.pop();
        A arg = unwrap<A>(w_arg);
        call_native(m, std::function<R(B...)>([f,arg](B...rest){ return f(rest...,arg); }));
    }

    template <class R, class...A>
    struct Primitive : public Value {
        using F = std::function<R(A...)>;
        F f;
        Primitive (F f_) : f(f_) {}
        Type* type () const override { return &Types::Operation; }
        void mutate (Machine& m) override {
            call_native(m, f);
        }
    };

    // Arguments can be deduced for free functions but not lambda expressions.
    template <class R, class...A>
    Primitive<R,A...>* new_Primitive (Machine& m, R(*f)(A...)) {
        return m.new_<Primitive<R,A...>>(f);
    }

    void Machine::raise (Error* error_) {
        this->error = error_;
    }

    void Machine::clear_error () {
        this->error = nullptr;
    }

    Error* Machine::get_error_or_null () const {
        return error;
    }
    size_t Machine::stack_size () const {
        return stack.size();
    }

    int ret1 () { return 1; }
    int add1 (int a) { return a + 1; }
    int muladd (int a, int b, int c) { return a * b + c; }
    int sub_int (int a, int b) { return a - b; }

    void
    Machine::load_standard_library () {
        bind("+", new_<Operation>([](Machine& s){
            Integer* b = s.pop()->be_integer_or_throw();
            Integer* a = s.pop()->be_integer_or_throw();
            s.new_<Integer>(*a + *b)->mutate(s);
        }));
        bind("-", new_<Operation>([](Machine& s){
            Integer* b = s.pop()->be_integer_or_throw();
            Integer* a = s.pop()->be_integer_or_throw();
            s.new_<Integer>(*a - *b)->mutate(s);
        }));
        bind("*", new_<Operation>([](Machine& s){
            Integer* b = s.pop()->be_integer_or_throw();
            Integer* a = s.pop()->be_integer_or_throw();
            s.new_<Integer>(*a * *b)->mutate(s);
        }));
        bind("collect_garbage", new_<Operation>([](Machine& s){
            s.collect_garbage();
        }));
        bind("show_gc_stats", new_<Operation>([](Machine& s){
            s.show_gc_stats();
        }));
        // DEBUG Example
        bind("ret1", new_Primitive(*this, &ret1));
        bind("add1", new_Primitive(*this, &add1));
        bind("muladd", new_Primitive(*this, &muladd));
    }

}
