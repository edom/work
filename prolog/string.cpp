// mutable byte string
class String final {
private:
    Array<char> array;
    size_t count () const { return array.count_; }
    size_t limit () const { return array.limit; }
public:
    // construction
        String (size_t limit) : array(limit) {}
        static String* copy_cstr (const char* cstr) {
            size_t limit = strlen(cstr);
            String* s = new String(limit);
            s->append_trunc(cstr);
            return s;
        }
    // delegates
        void resize_to (size_t new_limit) { array.resize_to(new_limit); }
        void clear () { array.clear(); }
        size_t write_to (FILE* out) const { return array.write_to(out); }
    // comparison
        bool equals (const String* that) const {
            assert(this->count() <= this->limit());
            assert(that->count() <= that->limit());
            if (this->count() != that->count()) {
                return false;
            }
            size_t i = 0;
            while (i < count()) {
                if (this->array[i] != that->array[i]) {
                    return false;
                }
                ++i;
            }
            return true;
        }
    // append
        size_t append_trunc (const String* that) {
            size_t i = 0;
            while (count() < limit() && i < that->count()) {
                array[count()] = that->array[i];
                ++array.count_;
                ++i;
            }
            return i;
        }
        size_t append_trunc (const char* cstr) {
            size_t i = 0;
            while (count() < limit() && cstr[i] != 0) {
                array[count()] = cstr[i];
                ++array.count_;
                ++i;
            }
            return i;
        }
        void printf (const char* format, ...) {
            va_list ap;
            va_start(ap, format);
            size_t n = vsnprintf(array.items + array.count_, array.limit - array.count_, format, ap);
            array.count_ += n;
            if (count() > limit()) {
                puts("String.printf: count_ > limit");
                abort();
            }
            va_end(ap);
        }
};
