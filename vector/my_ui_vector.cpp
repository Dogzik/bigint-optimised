#include "my_ui_vector.h"
#include <cassert>
typedef unsigned int ui;

size_t get_new_capacity(const size_t n) {
	if (n == 0) {
		return 4;
	}
	return (n * 3) >> 1;
}

ui* copy_data(const ui* src, size_t cnt, size_t cap) {
	ui* res = static_cast<ui*>(operator new(cap * sizeof(ui)));
	memcpy(res, src, cnt * sizeof(ui));
	memset(res + cnt, 0, (cap - cnt) * sizeof(ui));
	return res;
}

bool ui_vector::is_big() const {
	return (cur_data != _data.small_data);
}

size_t ui_vector::size() const {
	return _size;
}

ui_vector::ui_vector() : _size(0), cur_data(_data.small_data) {
	memset(cur_data, 0, SMALL_SIZE * sizeof(ui));
}

ui_vector::ui_vector(size_t nsize) : ui_vector() {
	reserve(nsize);
	_size = nsize;
}

size_t ui_vector::get_capacity() const {
	if (is_big()) {
		return _data.big_data.capacity;
	} else {
		return SMALL_SIZE;
	}
}

ui_vector::big::big(ui* a, size_t cap) : capacity(cap), ptr(a) {}

ui_vector::~ui_vector() {
	if (is_big()) {
		_data.big_data.~big();
	}
}

void ui_vector::make_big(size_t cap) {
	new(&_data.big_data) big(copy_data(cur_data, _size, cap), cap);
	cur_data = _data.big_data.ptr.get();
}

void ui_vector::set_capacity(size_t cap) {
	if (is_big() || (cap > SMALL_SIZE)) {
		if (!is_big()) {
			make_big(cap);
		} else {
			_data.big_data.ptr.reset(copy_data(cur_data, size(), cap), my_deleter());
			_data.big_data.capacity = cap;
			cur_data = _data.big_data.ptr.get();
		}
	}
}

void ui_vector::reserve(size_t cap) {
	if (cap > get_capacity()) {
		set_capacity(cap);
	}
}

void ui_vector::prepare_for_change() {
	if (is_big() && !_data.big_data.ptr.unique()) {
		_data.big_data.ptr.reset(copy_data(cur_data, size(), size()), my_deleter());
		_data.big_data.capacity = size();
		cur_data = _data.big_data.ptr.get();
	}
}

ui const& ui_vector::operator[](size_t ind) const {
	return cur_data[ind];
}

ui& ui_vector::operator[](size_t ind) {
	//prepare_for_change();
	assert(!(is_big() && !_data.big_data.ptr.unique()));
	return cur_data[ind];
}

bool operator==(const ui_vector &a, const ui_vector &b) {
	if (a._size != b._size) {
		return 0;
	}
	return (memcmp(a.cur_data, b.cur_data, a._size * sizeof(ui)) == 0);
}

void ui_vector::resize(size_t sz) {
	reserve(sz);
	_size = sz;
}

bool ui_vector::empty() {
	return (_size == 0);
}

void ui_vector::pop_back() {
	//prepare_for_change();
	_size--;
}

void ui_vector::push_back(const ui a) {
	if (get_capacity() < _size + 1) {
		reserve(get_new_capacity(_size));
	}
	//prepare_for_change();
	cur_data[_size] = a;
	_size++;
}

ui ui_vector::back() {
	return cur_data[_size - 1];
}

ui_vector::ui_vector(ui_vector const &other) : _size(other._size) {
	if (other.is_big()) {
		new (&_data.big_data) big(other._data.big_data);
		cur_data = _data.big_data.ptr.get();
	} else {
		memcpy(_data.small_data, other._data.small_data, SMALL_SIZE * sizeof(ui));
		cur_data = _data.small_data;
	}
}

void ui_vector::big::swap(big &other) noexcept {
	using std::swap;
	swap(ptr, other.ptr);
	swap(capacity, other.capacity);
}

void ui_vector::swap_big_small(typename ui_vector::any_data &a, typename ui_vector::any_data &b) noexcept {
	ui temp[SMALL_SIZE];
	memcpy(temp, b.small_data, SMALL_SIZE * sizeof(ui));
	new(&b.big_data) big(a.big_data);
	a.big_data.~big();
	memcpy(a.small_data, temp, SMALL_SIZE * sizeof(ui));
}

void ui_vector::swap(ui_vector &other) noexcept {
	using std::swap;
	if (!is_big() && !other.is_big()) {
		for (size_t i = 0; i < SMALL_SIZE; i++) {
			swap(_data.small_data[i], other._data.small_data[i]);
		}
	} else if (is_big() && other.is_big()) {
		swap(_data.big_data, other._data.big_data);
		cur_data = _data.big_data.ptr.get();
		other.cur_data = other._data.big_data.ptr.get();
	} else if (is_big()) {
		swap_big_small(_data, other._data);
		cur_data = _data.small_data;
		other.cur_data = other._data.big_data.ptr.get();
	} else {
		swap_big_small(other._data, _data);
		other.cur_data = other._data.small_data;
		cur_data = _data.big_data.ptr.get();
	}
	swap(_size, other._size);
}

ui_vector& ui_vector::operator=(ui_vector const &other) {
	ui_vector temp(other);
	swap(temp);
	return *this;
}
