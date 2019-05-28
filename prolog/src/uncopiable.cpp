#ifndef com_spacetimecat_prolog_uncopiable_cpp_included
#define com_spacetimecat_prolog_uncopiable_cpp_included

class Uncopiable {
    protected:
        Uncopiable () {}
    public:
        Uncopiable (const Uncopiable&) = delete;
        void operator= (const Uncopiable&) = delete;
};

#endif
