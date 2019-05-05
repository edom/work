class Uncopiable {
    protected:
        Uncopiable () {}
    public:
        Uncopiable (const Uncopiable&) = delete;
        void operator= (const Uncopiable&) = delete;
};
