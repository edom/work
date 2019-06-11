#ifndef READLINE_API_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define READLINE_API_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "std.h"

namespace Readline {

    using Interp_Impl::Std_String;

    using rl_compentry_func_t = char* (const char*, int);

    void init ();

    void set_completion_entry_generator (rl_compentry_func_t* f);

    bool getline (const Std_String& prompt, Std_String& out);

}

#endif
