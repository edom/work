#ifndef GC_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define GC_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include "ring.h"
#include "std.h"

namespace Interp_Impl {

    //  Something that can be garbage-collected.

    class GC_Object {

        friend class Objects;

        private:

            bool mark_;

        public:

            virtual ~GC_Object ();

            //  We use a technique similar to that in https://github.com/doublec/gc.

            void mark ();

            //  The programmer must remember to do these:
            //  -   Call "mark" for each instance of GC_Object that is directly referred to by this class.
            //  -   Call "mark" for each direct superclass that is a descendant of GC_Object.
            //
            //  Failing to do any of that will crash the program
            //  because an object will be deleted while it is still being used.

            virtual void mark_children ();

    };

    struct GC_Sample {
        using Count = std::size_t;

        time_t begin_time; // wall clock
        timespec t0; // before mark
        timespec t1; // after mark, before sweep
        timespec t2; // after sweep
        Count objects_before;
        Count objects_after;

        Std_String to_std_string () const;
    };

    class Objects final {

        private:

            using List = Std_Vector<GC_Object*>;

            //  List of all objects, including the roots.

            List objects;

            //  Roots must be a subset of objects.

            List roots;

            Ring<GC_Sample> gc_samples = Ring<GC_Sample>(16);

        public:

            ~Objects ();


            //  The bool return values should not be ignored.
            //  Systems without exceptions should return false
            //  to mean memory allocation failure.

            //  Every object must be added or pinned but not both.


            //  Users should not use this.
            //  Users should prefer the "new_" method template.
            //
            //  Make this Objects responsible for deleting the adoptee.
            //
            //  Each object must be added exactly once.
            //  If add(object) is never called, the object will leak.
            //  If add(object) is called more than once,
            //  the object will be deleted many times,
            //  and the program will crash at a weird time.

            bool add (GC_Object* object);

            //  Add to root list.
            //  The object must not have been "add"-ed first.

            bool pin (GC_Object* object);

            //  Return false if the object is not found in the list of pinned objects.

            bool unpin (GC_Object* object);

            //  Collect garbage using mark-and-sweep.
            //  Every object encountered while marking must have been "add"-ed;
            //  otherwise the program will crash at a weird time.

            void collect ();

            void show_gc_stats ();

            //  Instantiate and track for garbage collection.
            //  A good practice is to make T's constructors protected,
            //  and declare "friend class Objects" in T.

            template <typename T, typename ...Arg> T* new_ (Arg&&... arg) {
                T* inst = new T(std::forward<Arg>(arg)...);
                GC_Object* dummy = inst; // T must be a subtype of GC_Object
                (void) dummy;
                add(inst);
                return inst;
            }
    };
}

#endif
