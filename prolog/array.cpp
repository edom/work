class String;

template<typename T>
class Array {
protected:
    size_t count_;
    size_t limit;
    T* items;

    void require_min_limit (size_t min) {
        while (limit < min) {
            expand();
        }
    }
    void expand () {
        size_t new_limit = 2 * limit;
        if (new_limit <= limit) {
            puts("unsigned integer arithmetic overflow");
            abort();
        }
        resize_to(new_limit);
    }
    void resize_to (size_t new_limit) {
        T* new_items = new T[new_limit];
        size_t n = (count_ < new_limit) ? count_ : new_limit;
        for (size_t i = 0; i < n; ++i) {
            new_items[i] = items[i];
        }
        delete[] items;
        items = new_items;
    }
    size_t write_to (FILE* out) const {
        return fwrite(items, 1, count_, out);
    }
public:
    Array (size_t limit) {
        this->count_ = 0;
        this->limit = limit;
        this->items = new T[limit];
    }
    ~Array () {
        count_ = 0;
        limit = 0;
        delete[] items;
        items = nullptr;
    }
    void add (const T& item) {
        require_min_limit(count_ + 1);
        items[count_] = item;
        ++count_;
    }
    void clear () {
        count_ = 0;
    }
    size_t count () const {
        return count_;
    }
    void set_count (size_t count_) {
        assert(count_ <= limit);
        this->count_ = count_;
    }
    const T& at (size_t i) const {
        assert(i < limit);
        return items[i];
    }
    T& at (size_t i) {
        assert(i < limit);
        return items[i];
    }
    bool contains (const T& item) const {
        for (size_t i = 0; i < count; ++i) {
            if (items[i] == item) {
                return true;
            }
        }
        return false;
    }
    const T& operator[] (size_t i) const {
        return at(i);
    }
    T& operator[] (size_t i) {
        return at(i);
    }
};