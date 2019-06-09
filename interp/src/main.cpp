#include "pch.h"

#include "library.h"

#if WANT_READLINE
#include "readline_gnu.cpp"
#else
#include "readline_std.cpp"
#endif

#include <cassert>

namespace Interp_Test {

    //  Test for GC_Object::mark().

    struct A {
        bool a_called = false;
        virtual ~A () {}
        virtual void m () { a_called = true; }
    };
    struct B : public A {
        // B does not override m.
    };
    struct C : public B {
        bool c_called = false;
        void m () override {
            c_called = true;
            B::m(); // We want to know whether this is forwarded to A::m().
        }
    };

    void test_virtual_method_forwarding () {
        C c;
        assert(!c.a_called);
        assert(!c.c_called);
        c.m();
        assert(c.a_called);
        assert(c.c_called);
    }

}

namespace Interp_Impl {

    Value* value_from_token (Machine& m, const Token& t) {
        using Type = Token::Type;
        switch (t.type) {
            case Type::Number:
                return m.new_<Integer>(t.to_std_string());
            case Type::Identifier: {
                Std_String s = t.to_std_string();
                return m.lookup(s);
            }
            default:
                return nullptr;
        }
    }

    void sanity_check () {
        static_assert(sizeof(char) == 1, "sizeof(char) == 1");
        static_assert(CHAR_BIT == 8, "CHAR_BIT == 8");

        Interp_Test::test_virtual_method_forwarding();
    }

    static const char*
    help_message =
R"(
--help
--test
)";

    int main (int argc, char* argv[]) {

        (void) argc;
        (void) argv;

        sanity_check();

        bool help = false;
        bool test = false;

        for (int i = 1; i < argc; ++i) {
            char* arg = argv[i];

            if (false) { }
            else if (strcmp(arg, "--help") == 0) { help = true; }
            else if (strcmp(arg, "--test") == 0) { test = true; }
            else {
                printf("Unknown argument: %s\n", arg);
                return EXIT_FAILURE;
            }
        }

        if (help) {
            printf("%s", help_message);
            return EXIT_SUCCESS;
        }

        if (test) {
            run_tests();
            return EXIT_SUCCESS;
        }

        std::stringstream ss;
        Interp_Parser::Std_Input_Stream i (&ss);
        Interp_Parser::Parser p (&i);

        //  These has to be static, for interfacing with readline
        //  which expects a callback with C linkage.
        //  Perhaps we should use the alternative interface?

        static Machine m;
        static std::vector<Std_String>* matches = nullptr;

        m.load_standard_library();

        Readline::init();

        Readline::set_completion_entry_generator([](const char* prefix, int i) -> char* {
            if (i < 0) { abort(); }
            size_t n = i;
            if (n == 0) {
                if (matches != nullptr) { delete matches; }
                matches = new std::vector<Std_String>(m.find_keys_with_prefix(prefix));
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
                Std_String line;
                if (!Readline::getline("< ", line)) {
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
                } catch (Error& e) {
                    std::cout << "> ERROR: " << e.message() << "\n" << std::flush;
                }
            }

            std::cout << "> stack = [";
            m.print_stack();
            std::cout << "]\n" << std::flush;
        }
        return EXIT_SUCCESS;
    }

}

int main (int argc, char* argv[]) {
    return Interp_Impl::main(argc, argv);
}
