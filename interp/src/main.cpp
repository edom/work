#include "pch.h"
#include "library.h"

#if WANT_READLINE
#include "readline_gnu.h"
#else
#include "readline_std.h"
#endif

using Value = Machine::Value;

Value* value_from_token (Machine::Machine& m, const Token& t) {
    using Type = Token::Type;
    using Integer = Machine::Integer;
    switch (t.type) {
        case Type::Number:
            return m.new_<Integer>(t.to_std_string());
        case Type::Identifier: {
            std::string s = t.to_std_string();
            return m.lookup(s);
        }
        default:
            return nullptr;
    }
}

int main (int argc, char* argv[]) {
    (void) argc;
    (void) argv;

    Readline readline;
    std::stringstream ss;
    Parser::Std_Input_Stream i (&ss);
    Parser::Parser p (&i);
    static Machine::Machine m;
    static std::vector<std::string>* matches = nullptr;

    m.load_standard_library();

    readline.set_completion_entry_generator([](const char* prefix, int i) -> char* {
        if (i < 0) { abort(); }
        size_t n = i;
        if (n == 0) {
            if (matches != nullptr) { delete matches; }
            matches = new std::vector<std::string>();
            // Find all keys that begin with the prefix.
            size_t prefix_length = strlen(prefix);
            for (auto& pair : m.bindings) {
                const Machine::Key& key = pair.first;
                if (prefix_length > key.length) { continue; }
                if (memcmp(prefix, key.bytes, prefix_length) == 0) {
                    matches->push_back(key.to_std_string());
                }
            }
        }
        if (n >= matches->size()) {
            delete matches;
            matches = nullptr;
            return nullptr;
        }
        // readline expects the return value to be allocated with malloc.
        return strdup((*matches)[n].c_str());
    });

    std::cout << "Interpreter\n";

    // readline.set_completion_entry_generator()

    for (;;) {

        {
            std::string line;
            if (!readline.getline("< ", line)) {
                std::cout << "\n" << std::flush;
                break;
            }
            ss.str(line);
            ss.clear();
        }

        for (;;) {
            Token t = p.read_next_token();
            if (t.is_eof()) {
                break;
            }
            Value* value = value_from_token(m, t);
            if (value == nullptr) {
                continue;
            }
            try {
                value->mutate(m);
            } catch (Machine::Error& e) {
                std::cout << "> ERROR: " << e.message() << "\n" << std::flush;
            }
        }

        std::cout << "> stack = [";
        m.print_stack();
        std::cout << "]\n" << std::flush;
    }
    return EXIT_SUCCESS;
}
