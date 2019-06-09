//  Garbage-collectible Array of Object pointers.
//  T must be a subclass of Object.
template<typename T>
class Array_object final : public Object, public Array<T*> {
    protected:
        void mark_children () {
            for (size_t i = 0; i < this->count_; ++i) {
                if (this->items[i] == nullptr) { continue; }
                this->items[i]->mark();
            }
            Object::mark_children();
        }
};
