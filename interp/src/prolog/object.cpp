#include "../library.h"
#include <ctime>

namespace Interp_Prolog {

    using GC_Object = Interp_Impl::GC_Object;

    //  Garbage-collectible object.

    template<typename T>
    class Foreign_object final : public GC_Object {

        private:

            T* raw;

        public:

            virtual ~Foreign_object () {
                delete raw;
                raw = nullptr;
            }

    };

}
