#include <cstdio>

#include "../array.h"

namespace Interp_Prolog {

    class Input_stream {
        private:
            FILE* handle;
            Array<unsigned char> back;
        public:
            int read_byte () {
                return fgetc(handle);
            }
    };

}
