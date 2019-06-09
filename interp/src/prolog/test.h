#ifndef PROLOG_TEST_H_INCLUDED
#define PROLOG_TEST_H_INCLUDED

#include <functional>

struct Test final {
    typedef std::function<bool()> run_t;

    const char* name;
    const run_t run;

    Test () {}

    Test (const char* name_, run_t run_)
    : name(name_)
    , run(run_)
    {
    }
};

#endif
