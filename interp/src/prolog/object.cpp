#include <time.h>

class Marked {
    public:
        virtual ~Marked () {}
        virtual bool is_live () = 0;
        virtual void set_live (bool) = 0;

        void
        mark () {
            if (is_live()) { return; }
            set_live(true);
            mark_children();
        }

    protected:
        // The programmer must call the mark_children methods of each direct superclass.
        virtual void mark_children () {}
};

//  Garbage-collectible object.
//  We use the same technique used in https://github.com/doublec/gc.
class Object : public Marked {
    private:

        bool live;

    public:

        Object () : live(false) {}

        bool is_live () override { return live; }
        void set_live (bool live) override { this->live = live; }
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
