class Garbage_collection final {

    static constexpr char DEAD = Object::DEAD;
    static constexpr char LIVE = Object::LIVE;

    int verbosity;

    void mark_objects_reachable_from (size_t depth, Object* root) {
        if (root == nullptr) { return; }
        if (root->mark == LIVE) { return; }
        myprintf(VERBOSITY_TRACE, "%*sMarking %p\n", (int)depth, "", root);
        root->mark = LIVE;
        Object::Refs refs = root->get_gc_out_refs();
        for (size_t j = 0; j < refs.count; ++j) {
            Object* child = refs[j];
            if (child == nullptr) { continue; }
            myprintf(VERBOSITY_TRACE, "%*s[%zu/%zu] %p refers to %p\n", (int)depth, "", j, refs.count, root, child);
            mark_objects_reachable_from(depth + 1, child);
        }
    }

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
    void delete_objects_unreachable_from (Array<Object*>& objects, Array<Object*>& roots) {
        struct timespec t0;
        struct timespec t1;
        if (clock_gettime(CLOCK_MONOTONIC, &t0) != 0) {
            perror("Garbage_collection.delete_objects_unreachable_from");
            abort();
        }
        myprintf(VERBOSITY_SUMMARY,
            "---------- Begin mark-and-sweep stop-the-world garbage collection\n"
            "---------- MARK phase: Determining the reachability of %zu objects from %zu roots\n",
                objects.count(),
                roots.count()
        );
        for (size_t i = 0; i < roots.count(); ++i) {
            Object* root = roots[i];
            mark_objects_reachable_from(0, root);
        }
        myprintf(VERBOSITY_SUMMARY,
            "---------- SWEEP phase: Deleting unreachable objects from %zu objects\n",
                objects.count()
        );
        size_t old_count = objects.count();
        size_t new_count = 0;
        {
            for (size_t i = 0; i < old_count; ++i) {
                Object* obj = objects[i];
                if (obj->mark == DEAD) {
                    printf("Deleting unreachable object %p\n", obj);
                    delete obj;
                    objects[i] = nullptr;
                } else {
                    obj->mark = DEAD;
                    objects[new_count] = obj;
                    ++new_count;
                }
            }
            objects.set_count(new_count);
        }
        myprintf(VERBOSITY_SUMMARY,
            "---------- End garbage collection\n"
            "Previous object count: %zu\n"
            " Current object count: %zu\n",
                old_count,
                new_count
        );
        if (clock_gettime(CLOCK_MONOTONIC, &t1) != 0) {
            perror("Garbage_collection.delete_objects_unreachable_from");
            abort();
        }
        constexpr int64_t E3 = 1000;
        constexpr int64_t E9 = 1000000000;
        int64_t micro = ((t1.tv_sec*E9 + t1.tv_nsec) - (t0.tv_sec*E9 + t0.tv_nsec)) / E3;
        myprintf(VERBOSITY_SUMMARY,
            "         Elapsed time: %" PRId64 " microseconds\n",
                micro
        );
    }
};
