#include "pch.h"

#include <readline/readline.h>
#include <readline/history.h>

#include "readline_api.h"

namespace Readline {

    static const char*
    history_file = "interpreter_history";

    void
    init () {
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
    getline (const Std_String& prompt, Std_String& out) {
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
