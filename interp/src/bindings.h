#ifndef BINDINGS_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define BINDINGS_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

namespace Machine {

    struct Key {
        // A char* points to a null-terminated string.
        // A byte* points to a byte string that is not necessarily null-terminated.
        using byte = char;

        size_t hash; // precomputed hash
        size_t length; // byte count
        byte* bytes; // constant through this object's lifetime; not null-terminated

        Key (const byte* bytes_)
        : Key (bytes_, strlen(bytes_)) {
        }

        Key (const byte* bytes_, size_t length_);

        Key (const Key& that)
        : hash(that.hash)
        , length(that.length)
        {
            bytes = new byte[length];
            memcpy(bytes, that.bytes, length);
        }

        Key (Key&& that)
        : hash(that.hash)
        , length(that.length)
        , bytes(that.bytes) {
            that.bytes = nullptr;
        }

        ~Key () {
            if (bytes != nullptr) { delete[] bytes; }
        }

        static size_t compute_hash (const byte* bytes, size_t length);

        int compare (const Key& that) const;

        bool operator== (const Key& that) const;
        bool operator!= (const Key& that) const;
        bool operator<= (const Key& that) const;
        bool operator< (const Key& that) const;

        std::string to_std_string () const {
            return std::string(bytes, length);
        }

        struct Hasher {
            size_t operator() (const Key& that) const {
                return that.hash;
            }
        };
    };

    class Value;

    struct Bindings {
        using Map = std::unordered_map<Key, Value*, Key::Hasher>;
        using iterator = Map::iterator;

        Map bindings;

        void bind (const Key& key, Value* value) { bindings[key] = value; }
        void bind (Key&& key, Value* value) { bindings[key] = value; }

        Value* lookup (const Key& key) const;

        iterator begin () { return bindings.begin(); }
        iterator end () { return bindings.end(); }
    };

}

#endif
