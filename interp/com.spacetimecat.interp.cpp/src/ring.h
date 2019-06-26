#ifndef RING_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define RING_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

namespace Interp_Impl {
    class Ring_ {
        protected:
            using N = std::size_t;

            N stride_;
            N capacity_;
            N begin_ = 0;
            N size_ = 0;
            char* buffer;

            Ring_ (N stride, N capacity);
            ~Ring_ ();
    };

    template<typename E>
    class Ring : private Ring_ {
        private:
            E& physical (N i) {
                return *(E*)(buffer + i*stride_);
            }

            E& logical (N i) {
                return physical((begin_ + i) % capacity_);
            }

        public:
            Ring (N capacity)
            : Ring_(sizeof(E), capacity)
            { }

            // Overwrite if full.
            void push_back (const E&& that) {
                E& slot = logical(size_);
                if (size_ < capacity_) {
                    ++size_;
                } else {
                    physical(begin_).~E();
                    begin_ = (begin_ + 1) % capacity_;
                }
                new (&slot) E(that);
            }

            E& operator[] (N i) {
                return logical(i);
            }

            friend struct iterator;

            struct iterator {
                Ring& ring;
                N offset; // logical

                E& operator* () {
                    return ring[offset];
                }

                bool operator== (const iterator& that) {
                    return this->offset == that.offset;
                }

                bool operator!= (const iterator& that) {
                    return !(*this == that);
                }

                iterator& operator++ () {
                    ++offset;
                    return *this;
                }
            };

            iterator begin () {
                return iterator{*this, 0};
            }
            iterator end () {
                return iterator{*this, size_};
            }
    };
}

#endif
