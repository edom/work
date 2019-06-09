#include "pch.h"

#include "readline_api.h"

namespace Readline {

    void
    set_completion_entry_generator (rl_compentry_func_t* f) {
        (void) f;
    }

    bool
    getline (const Std_String& prompt, Std_String& out) {
        if (std::cin.eof()) {
            return false;
        }
        std::cout << prompt << std::flush;
        std::getline(std::cin, out);
        out += "\n";
        return true;
    }

};
