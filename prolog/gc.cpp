#include <functional>

class Garbage_collection final {

    int verbosity;

    int myprintf (int min_verbosity, const char* format, ...) {
        if (verbosity < min_verbosity) { return 0; }
        va_list ap;
        va_start(ap, format);
        int n = vprintf(format, ap);
        va_end(ap);
        return n;
    }
public:
    static constexpr int VERBOSITY_QUIET = 0;
    static constexpr int VERBOSITY_SUMMARY = 100;
    static constexpr int VERBOSITY_TRACE = 200;
    Garbage_collection () {
        this->verbosity = VERBOSITY_QUIET;
    }
    void set_verbosity (int verbosity) {
        this->verbosity = verbosity;
    }
    // roots may contain nullptr
    void delete_objects_unreachable_from (Array<Object*>& objects, Array<Object*>& roots) {
        struct timespec t0;
        struct timespec t1;
        if (clock_gettime(CLOCK_MONOTONIC, &t0) != 0) {
            perror("Garbage_collection.delete_objects_unreachable_from");
            abort();
        }
        {
            myprintf(VERBOSITY_SUMMARY,
                "---------- Begin mark-and-sweep stop-the-world garbage collection\n"
                "---------- MARK phase: Determining the reachability of %zu objects from %zu roots\n",
                    objects.count(),
                    roots.count()
            );
            int64_t ms = milliseconds_taken_by([&](){
                for (size_t i = 0; i < roots.count(); ++i) {
                    Object* root = roots[i];
                    if (root == nullptr) { continue; }
                    root->mark();
                }
            });
            myprintf(VERBOSITY_SUMMARY,
                "---------- MARK phase took %zu milliseconds\n",
                    ms
            );
        }
        myprintf(VERBOSITY_SUMMARY,
            "---------- SWEEP phase: Deleting unreachable objects from %zu objects\n",
                objects.count()
        );
        size_t old_count = objects.count();
        size_t new_count = 0;
        {
            int64_t ms = milliseconds_taken_by([&](){
                for (size_t i = 0; i < old_count; ++i) {
                    Object* obj = objects[i];
                    if (obj->is_live()) {
                        obj->set_live(false);
                        objects[new_count] = obj;
                        ++new_count;
                    } else {
                        delete obj;
                        objects[i] = nullptr;
                    }
                }
                objects.set_count(new_count);
            });
            myprintf(VERBOSITY_SUMMARY,
                "---------- SWEEP phase took %zu milliseconds\n",
                    ms
            );
        }
        myprintf(VERBOSITY_SUMMARY,
            "---------- End garbage collection\n"
            "Previous object count: %zu\n"
            " Deleted object count: %zu\n"
            " Current object count: %zu\n",
                old_count,
                old_count - new_count,
                new_count
        );
        if (clock_gettime(CLOCK_MONOTONIC, &t1) != 0) {
            perror("Garbage_collection.delete_objects_unreachable_from");
            abort();
        }
        constexpr int64_t E6 = 1000000;
        constexpr int64_t E9 = 1000000000;
        int64_t milli = ((t1.tv_sec*E9 + t1.tv_nsec) - (t0.tv_sec*E9 + t0.tv_nsec)) / E6;
        myprintf(VERBOSITY_SUMMARY,
            "         Elapsed time: %" PRId64 " milliseconds\n",
                milli
        );
    }
private:
    static int64_t milliseconds_taken_by (std::function<void()> f) {
        struct timespec t0;
        struct timespec t1;
        if (clock_gettime(CLOCK_MONOTONIC, &t0) != 0) {
            perror("Garbage_collection.milliseconds_taken_by");
            abort();
        }
        f();
        if (clock_gettime(CLOCK_MONOTONIC, &t1) != 0) {
            perror("Garbage_collection.milliseconds_taken_by");
            abort();
        }
        constexpr int64_t E6 = 1000000;
        constexpr int64_t E9 = 1000000000;
        return ((t1.tv_sec*E9 + t1.tv_nsec) - (t0.tv_sec*E9 + t0.tv_nsec)) / E6;
    }
};
