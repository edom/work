#ifndef GC_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a
#define GC_H_INCLUDED_83064c68_bded_42fa_98a4_2d49da24263a

#include "pch.h"

#include "ring.h"
#include "timer.h"

namespace Machine {
    // Something that can be garbage-collected.
    class GC_Object {
        friend class Objects;
        private:
            char mark_;
        public:
            virtual
            ~GC_Object () {}
        protected:
            virtual void
            mark () {
                mark_ = 1;
            }
    };

    class Objects final {
        private:
            using List = std::vector<GC_Object*>;

            struct GC_Sample {
                using Count = std::size_t;

                time_t begin_time;
                timespec t0;
                timespec t1;
                Count objects_before;
                Count objects_after;

                std::string to_std_string () const {
                    std::ostringstream s;
                    Count objects_collected = objects_before - objects_after;
                    char b_time [64];
                    const char* s_begin_time;
                    {
                        tm the_tm;
                        localtime_r(&begin_time, &the_tm);
                        if (strftime(b_time, sizeof(b_time), "%F %T %Z", &the_tm) != 0) {
                            s_begin_time = b_time;
                        } else {
                            s_begin_time = "(strftime failed)";
                        }
                    }
                    s   << "started on " << s_begin_time << ", "
                        << "took " << (t1 - t0) << " µs, "
                        <<  objects_before << "-" << objects_collected << "=" << objects_after << " objects";
                    return s.str();
                }
            };

            List objects;
            Ring<GC_Sample> gc_samples = {16};

        public:
            void add (GC_Object* object) {
                objects.push_back(object);
            }

            // Collect garbage using mark-and-sweep.
            void
            delete_unreachable_from (GC_Object& root) {
                time_t begin_time;
                timespec t0;
                timespec t1;

                begin_time = time(nullptr);
                clock_gettime(CLOCK_MONOTONIC, &t0);

                for (auto object : objects) {
                    object->mark_ = 0;
                }

                root.mark();

                std::size_t num_original = objects.size();
                auto begin = objects.begin();
                auto live = begin;
                for (auto suspect : objects) {
                    if (suspect->mark_ == 0) {
                        delete suspect;
                    } else {
                        *live = suspect;
                        ++live;
                    }
                }
                objects.resize(live - objects.begin());

                clock_gettime(CLOCK_MONOTONIC, &t1);

                gc_samples.push_back({begin_time, t0, t1, num_original, objects.size()});
            }

            void
            show_gc_stats () {
                for (auto& sample : gc_samples) {
                    std::cout << sample.to_std_string() << "\n";
                }
            }
    };
}

#endif
