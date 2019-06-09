#ifndef INTEGER_GMP_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define INTEGER_GMP_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include <gmp.h>

namespace Integer {
    // The move-constructor and the destructor depend on GMP internals.
    class Integer final {
        private:
            mpz_t m;
        public:
            Integer () { mpz_init(m); }
            Integer (long that) { mpz_init_set_si(m, that); }
            Integer (const Integer& that) { mpz_init_set(m, that.m); }
            Integer (Integer&& that) {
                this->m[0] = that.m[0];
                that.m[0]._mp_d = nullptr;
            }
            Integer (const std::string& s) : Integer(s.c_str()) { }
            Integer (const char* str) : Integer(str, 10) { }
            Integer (const char* str, int base) { mpz_init_set_str(m, str, base); }
            ~Integer () {
                if (m[0]._mp_d != nullptr) {
                    mpz_clear(m);
                    m[0]._mp_d = nullptr;
                }
            }

            bool to_int (int& i) const {
                if (!mpz_fits_sint_p(m)) {
                    return false;
                }
                i = mpz_get_si(m);
                return true;
            }

            std::string to_std_string () const {
                const int base = 10;
                const size_t n = mpz_sizeinbase(m, base) + 2; // sign and null-terminator
                char* cp = new char[n];
                mpz_get_str(cp, base, m);
                std::string str (cp);
                delete[] cp;
                return str;
            }

            int compare (const Integer& that) const { return mpz_cmp(this->m, that.m); }
            bool operator< (const Integer& that) const { return this->compare(that) < 0; }
            bool operator> (const Integer& that) const { return this->compare(that) > 0; }
            bool operator<= (const Integer& that) const { return this->compare(that) <= 0; }
            bool operator>= (const Integer& that) const { return this->compare(that) >= 0; }
            bool operator== (const Integer& that) const { return this->compare(that) == 0; }
            bool operator!= (const Integer& that) const { return this->compare(that) != 0; }
            Integer& operator= (const Integer& that) {
                mpz_set(this->m, that.m);
                return *this;
            }
            Integer operator+ (const Integer& that) const {
                Integer result;
                mpz_add(result.m, this->m, that.m);
                return result;
            }
            Integer operator- (const Integer& that) const {
                Integer result;
                mpz_sub(result.m, this->m, that.m);
                return result;
            }
            Integer operator* (const Integer& that) const {
                Integer result;
                mpz_mul(result.m, this->m, that.m);
                return result;
            }
    };
}

#endif
