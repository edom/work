#ifndef STD_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define STD_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

//  The purpose of these using-directives is to enable us to
//  swap std containers classes with something that does not use exceptions,
//  in the future, in principle.

namespace Interp_Impl {

    template <typename T>
    using Std_Vector = std::vector<T>;

    using Std_String = std::string;

    template <typename K, typename V, typename H>
    using Std_Unordered_Map = std::unordered_map<K, V, H>;

}

#endif
