#ifndef TEST_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define TEST_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "std.h"

//  C++11 test framework with closures instead of macros.

#include <functional>

namespace Interp_Impl {

    //  Use with lambda closures.

    struct Test final {

        using Action = std::function<bool()>;

        const char* name;
        const Action run;

        Test (const char* name_, Action run_)
        : name(name_)
        , run(run_)
        {
        }

    };

    void run (const Std_Vector<Test>& tests);

    //  Internal.

    void run_tests ();

}

#endif
