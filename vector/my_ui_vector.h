#ifndef MY_UI_VECTOR
#define MY_UI_VECTOR

#include <cstdlib>
#include <memory>
#include <cstring>
struct ui_vector {
public:
	typedef unsigned int ui;

	ui_vector();
	~ui_vector();
	ui_vector(size_t nsize);
	ui_vector(ui_vector const &other);

	void reserve(size_t cap);
	void resize(size_t sz);
	size_t size() const;
	bool empty();
	//ui* get_data();

	ui& operator[](size_t ind);
	ui const& operator[](size_t ind) const;

	ui_vector& operator=(ui_vector const &other);

	void pop_back();
	void push_back(const ui a);
	ui back();

	void swap(ui_vector &other) noexcept;
	friend bool operator==(const ui_vector &a, const ui_vector &b);
	void prepare_for_change();
private:
	size_t get_capacity() const;
	static const size_t SMALL_SIZE = 4;
	size_t _size;
	struct big {
		size_t capacity;
		std::shared_ptr<ui> ptr;
		big(ui* a, size_t cap);
		void swap(big &other) noexcept;
	};

	union any_data {
		ui small_data[SMALL_SIZE];
		big big_data;
		any_data() {};
		~any_data() {};
	} _data;

	struct my_deleter {
		void operator()(ui* p) {
			operator delete(p);
		}
	};

	ui* cur_data;

	bool is_big() const;
	void make_big(size_t cap);
	void set_capacity(size_t cap);

	void swap_big_small(any_data &a, any_data &b) noexcept;
};

#endif 

