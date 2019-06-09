#ifndef INTEGER_STD_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define INTEGER_STD_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include "std.h"

namespace Interp_Integer {

    using Interp_Impl::Std_String;

    class Integer final {
        private:
            using Int = int;
            Int m;
        public:
            Integer () { }
            Integer (Int that) : m(that) { }
            Integer (const Integer& that) : m(that.m) { }
            Integer (const Std_String& s) : m(std::stoi(s)) { }
            Integer (const char* str) : Integer(str, 10) { }
            Integer (const char* str, int base) : m(std::stoi(str, nullptr, base)) {}

            bool to_int (int& i) const {
                i = m;
                return true;
            }

            Std_String to_std_string () const {
                return std::to_string(m);
            }

            int compare (const Integer& that) const { return this->m - that.m; }
            bool operator< (const Integer& that) const { return this->m < that.m; }
            bool operator> (const Integer& that) const { return this->m > that.m; }
            bool operator<= (const Integer& that) const { return this->m <= that.m; }
            bool operator>= (const Integer& that) const { return this->m >= that.m; }
            bool operator== (const Integer& that) const { return this->m == that.m; }
            bool operator!= (const Integer& that) const { return this->m != that.m; }
            Integer& operator= (const Integer& that) { m = that.m; return *this; }
            Integer operator+ (const Integer& that) const { return {this->m + that.m}; }
            Integer operator- (const Integer& that) const { return {this->m - that.m}; }
            Integer operator* (const Integer& that) const { return {this->m * that.m}; }
    };

}

#endif
