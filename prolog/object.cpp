#include <time.h>

// garbage-collectible object
class Object {
    friend class Garbage_collection;

    static constexpr char DEAD = 0;
    static constexpr char LIVE = 1;

    char mark;

    static constexpr Object* AT_NONE (Object* _this, size_t _i) { return nullptr; }

protected:

    Object () : mark(DEAD) {}

    struct Refs;

    virtual Refs get_gc_out_refs () = 0;
    virtual ~Object () {}

    // An instance of "Refs" iterates the objects pointed/used/referred by "self".
    struct Refs final {
        typedef Object* at_t (Object*, size_t);

        Object* self;
        size_t count;
        at_t* at;

        Refs (Object* self, size_t count, at_t* at) {
            this->self = self;
            this->count = count;
            this->at = at;
        }

        // may return nullptr
        Object* operator[] (size_t i) {
            return (*at)(self, i);
        };
    };

    static Refs REFS_NONE (Object* self) {
        return Refs(self, 0, AT_NONE);
    }
};

template<typename T>
class Foreign_object final : public Object {
private:
    T* raw;
    Refs get_gc_out_refs () override {
        return REFS_NONE(this);
    }
public:
    virtual ~Foreign_object () {
        delete raw;
        raw = nullptr;
    }
};
