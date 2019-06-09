#include "pch.h"

#include <cassert>

#include "library.h"
#include "gc.h"

namespace Interp_Impl {

    // -------------------- memory management

    GC_Object::~GC_Object () {
    }

    void
    GC_Object::mark () {
        //  This conditional reduces the duplication of work
        //  in the marking of a densely connected object graph.
        //
        //  The trade off is that all objects that have children must be added to Objects,
        //  either as root or non-root.
        if (!this->mark_) {
            this->mark_ = true;
            this->mark_children();
        }
    }

    void
    GC_Object::mark_children () {
    }

    Std_String
    GC_Sample::to_std_string () const {
        std::ostringstream s;
        tm local_time;
        to_local_time(begin_time, local_time);
        Count objects_collected = objects_before - objects_after;
        s   << "started on " << format_time("%F %T %Z", local_time) << ", "
            << "marking took " << (t1 - t0) << " µs, "
            << "sweeping took " << (t2 - t1) << " µs, "
            << objects_collected << " of " << objects_before << " objects collected";
        return s.str();
    }

    Objects::~Objects() {
        for (auto object : objects) {
            delete object;
        }
    }

    bool
    Objects::add (GC_Object* object) {
        assert(object != nullptr);
        objects.push_back(object);
        return true;
    }

    bool
    Objects::pin (GC_Object* object) {
        assert(object != nullptr);
        roots.push_back(object);
        return true;
    }

    bool
    Objects::unpin (GC_Object* object) {
        for (auto& root : roots) {
            if (root == object) {
                root = nullptr;
                return true;
            }
        }
        return false;
    }

    void
    Objects::collect () {

        time_t begin_time;
        timespec t0;
        timespec t1;
        timespec t2;

        begin_time = time(nullptr);

        // -------------------- mark

        get_monotonic_clock(t0);

        for (auto object : objects) {
            object->mark_ = false;
        }

        {
            auto begin = roots.begin();
            auto live = begin;
            for (auto i = begin; i != roots.end(); ++i) {
                auto root = *i;
                if (root != nullptr) {
                    root->mark_ = false;
                    if (live != i) {
                        *live = root;
                    }
                    ++live;
                }
            }
            roots.resize(live - begin);
        }

        for (auto root : roots) {
            root->mark();
        }

        // -------------------- sweep

        get_monotonic_clock(t1);

        std::cout << "--------------------\n";

        std::size_t old_count = objects.size();
        std::size_t new_count;
        {
            auto begin = objects.begin();
            auto end = objects.end();
            auto live = begin;
            for (auto i = begin; i != end; ++i) {
                auto suspect = *i;
                // DEBUG
                std::cout << suspect << " " << suspect->mark_ << "\n";
                if (suspect->mark_) {
                    if (live != i) {
                        *live = suspect;
                    }
                    ++live;
                } else {
                    std::cout << "delete " << suspect << "\n";
                    delete suspect;
                }
            }
            new_count = live - begin;
        }

        //  Shrink the array of objects.

        std::size_t capacity = objects.capacity();
        if (new_count < capacity/4) {
            //  Problem: This is slow if the program has a loop that
            //  produces a lot of garbage and manually calls garbage collection.
            //  Ideally, we should only shrink "objects"
            //  when the operating system indicates that there is memory pressure.
            std::size_t new_capacity = capacity/2;
            std::size_t min_capacity = std::max<std::size_t>(1024, new_count);
            if (new_capacity >= min_capacity) {
                objects.resize(new_capacity);
                objects.shrink_to_fit();
            }
        }

        //  Delete the dangling references to deleted objects.

        objects.resize(new_count);

        // -------------------- end of garbage collection

        get_monotonic_clock(t2);

        gc_samples.push_back(GC_Sample({ begin_time, t0, t1, t2, old_count, new_count }));

    }

    void
    Objects::show_gc_stats () {
        for (auto& sample : gc_samples) {
            std::cout << sample.to_std_string() << "\n";
        }
    }

}
