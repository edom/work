// mutable byte string
// TODO make Array extend Term
class String final : public Term, private Array<char> {
    friend class World;
public:
    // construction
        String (size_t limit) : Array(limit) {}
        static String* copy_cstr (const char* cstr) {
            size_t limit = strlen(cstr);
            String* s = new String(limit);
            s->append_trunc(cstr);
            return s;
        }
        String* copy () const {
            String* c = new String(count_);
            for (size_t i = 0; i < count_; ++i) {
                c->items[i] = items[i];
            }
            c->count_ = count_;
            return c;
        }
    // delegates
        using Array::clear;
        using Array::write_to;
    // comparison
        bool equals (const String* that) const {
            assert(this->count_ <= this->limit);
            assert(that->count_ <= that->limit);
            if (this->count_ != that->count_) {
                return false;
            }
            size_t i = 0;
            while (i < count_) {
                if ((*this)[i] != (*that)[i]) {
                    return false;
                }
                ++i;
            }
            return true;
        }
    // append
        size_t append_trunc (const String* that) {
            size_t i = 0;
            while (count_ < limit && i < that->count_) {
                (*this)[count_] = (*that)[i];
                ++count_;
                ++i;
            }
            return i;
        }
        size_t append_trunc (const char* cstr) {
            size_t i = 0;
            while (count_ < limit && cstr[i] != 0) {
                (*this)[count_] = cstr[i];
                ++count_;
                ++i;
            }
            return i;
        }
        void printf (const char* format, ...) {
            va_list ap;
            va_start(ap, format);
            size_t n = vsnprintf(items + count_, limit - count_, format, ap);
            count_ += n;
            if (count_ > limit) {
                puts("String.printf: count_ > limit");
                abort();
            }
            va_end(ap);
        }
    // Term
        bool get_string (const String** val) const override {
            *val = this;
            return true;
        }
protected:
    void do_print_debug (String* out) const override {
        out->append_trunc(this);
    }
};
