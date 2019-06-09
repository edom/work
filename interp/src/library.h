#ifndef LIBRARY_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define LIBRARY_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "../build/config.h"

#include "std.h"

namespace Interp_Impl {

    // -------------------- enhanced C functions

    //  What should these confusing names have been: time_t, tm, timespec?

    //  Always null-terminate dst.
    //  n must be positive.
    //  Return dst.
    char*           safe_strncpy (char* dst, const char* src, size_t n);
    void            safe_strftime (char* buffer, size_t buffer_size, const char* format, const tm* time);
    void            to_local_time (const time_t& in, tm& out);

    Std_String      format_time (const char* format, const tm& time);

    //  abort if CLOCK_MONOTONIC is not available.
    void            get_monotonic_clock (timespec& t);

    using Micro = std::int64_t;

    Micro operator- (const timespec& a, const timespec& b);

}

#include "machine.h"
#include "parser.h"
#include "value.h"

#include "test.h"

#endif
