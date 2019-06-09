#ifndef READLINE_STD_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define READLINE_STD_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

struct Readline {
    using rl_compentry_func_t = char* (const char*, int);

    void
    set_completion_entry_generator (rl_compentry_func_t* f) {
    }

    bool
    getline (const std::string& prompt, std::string& out) {
        if (std::cin.eof()) {
            return false;
        }
        std::cout << prompt << std::flush;
        std::getline(std::cin, out);
        out += "\n";
        return true;
    }
};

#endif
