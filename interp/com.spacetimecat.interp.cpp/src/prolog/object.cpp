#include "term.h"

namespace Interp_Prolog {

    Foreign_Object::Foreign_Object (void* raw_)
    : raw(raw_)
    {
    }

    Foreign_Object::~Foreign_Object () {
    }

}
