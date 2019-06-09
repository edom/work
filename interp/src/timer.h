#ifndef TIMER_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define TIMER_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

namespace Machine {
    using Micro = std::int64_t;

    Micro operator- (const timespec& a, const timespec& b);
}

#endif
