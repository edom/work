#ifndef READLINE_GNU_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define READLINE_GNU_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include <readline/readline.h>
#include <readline/history.h>

struct Readline {

    const char* history_file = "interpreter_history";

    Readline () {
        using_history();
        if (read_history(history_file) != 0) {
            if (write_history(history_file) != 0) {
                std::cerr << "could not open history file: " << history_file << "\n";
            }
        }
    }

    void
    set_completion_entry_generator (rl_compentry_func_t* f) {
        rl_completion_entry_function = f;
    }

    bool
    getline (const std::string& prompt, std::string& out) {
        char* buf = readline(prompt.c_str());
        if (buf == nullptr) {
            return false;
        }
        if (*buf != 0) { // skip empty line
            add_history(buf);
            if (append_history(1, history_file) != 0) {
                if (write_history(history_file) != 0) {
                    std::cerr << "could not open history file: " << history_file << "\n";
                }
            }
        }
        out = buf;
        out += "\n";
        free(buf);
        return true;
    }
};

#endif
