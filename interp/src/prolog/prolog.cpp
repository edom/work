/*
Problems:
    - garbage collection not implemented
*/

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "uncopiable.cpp"
#include "object.cpp"
#include "array.cpp"
#include "gc.cpp"
#include "object_array.cpp"
#include "term.cpp"
#include "term_string.cpp"
#include "term_compound.cpp"
#include "world.h"
#include "term_integer.cpp"
#include "term_var.cpp"
#include "frame.cpp"
#include "fiber.cpp"
#include "world.cpp"
#include "call.cpp"
#include "test.h"
#include "parser.cpp"
#include "stream.cpp"
#include "test.cpp"

static const char* help_message =
R"(
--help
--test
)";

int
main (int argc, char* argv[]) {

    bool help = false;
    bool test = false;

    for (int i = 1; i < argc; ++i) {
        char* arg = argv[i];

        if (false) { } // dummy for formatting
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

    return EXIT_SUCCESS;
}
