#include <time.h>

//  Garbage-collectible object.
//  We use the same technique used in https://github.com/doublec/gc.
class Object {
    friend class Garbage_collection;

private:

    bool live;

protected:

    Object () : live(false) {}

    virtual ~Object () {}

    //  The programmer must call the mark_children methods of each direct superclass.
    virtual void mark_children () {}

public:

    void mark () {
        if (live) { return; }
        live = true;
        mark_children();
    }
};

template<typename T>
class Foreign_object final : public Object {
private:
    T* raw;
public:
    virtual ~Foreign_object () {
        delete raw;
        raw = nullptr;
    }
};
