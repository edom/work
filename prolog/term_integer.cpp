// Immutable integer with lots of overhead.
class Integer final : public Term {
    friend Integer* World::new_integer (intptr_t);
    private:
        const intptr_t i;
        Integer (intptr_t i_) : i(i_) {}
    public:
        bool get_integer (intptr_t* val) const override {
            *val = i;
            return true;
        }
    protected:
        void do_print_debug (String* out) const override {
            out->printf("%" PRIdPTR, i);
        }
};
