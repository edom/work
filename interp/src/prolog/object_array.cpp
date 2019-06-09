//  Garbage-collectible Array of Object pointers.
//  T must be a subclass of Object.
namespace Interp_Prolog {
    template<typename T>
    class Array_object final : public GC_Object, public Array<T*> {
        public:
            void mark_children () override {
                for (size_t i = 0; i < this->count_; ++i) {
                    if (this->items[i] == nullptr) { continue; }
                    this->items[i]->mark();
                }
                GC_Object::mark_children();
            }
    };
}
