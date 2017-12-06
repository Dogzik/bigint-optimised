#include "big_integer.h"
#include <algorithm>
#include <stdexcept>
#include <cassert>
//#include <vld.h>

typedef unsigned int ui;
typedef unsigned long long ull;

const ui MAX_DIGIT = UINT32_MAX;
const ui BASE = 32;
const int BASE_10 = 1000000000;

template<typename T>
ui my_ui_cast(T x) {
	return static_cast<ui>(x & MAX_DIGIT);
}

template<typename T>
ull my_ull_cast(T x) {
	return static_cast<ull>(x);
}

bool big_integer::is_zero() const
{
	return (!sign) && (length() == 0);
}

size_t big_integer::length() const {
	return data.size();
}

bool big_integer::is_negative() const {
	return sign;
}

big_integer big_integer::abs() const {
	return sign ? -(*this) : *this;
}

void big_integer::swap(big_integer &other) noexcept {
	std::swap(data, other.data);
	std::swap(sign, other.sign);
}

ui big_integer::get_real_digit(size_t ind) const {
	return data[ind];
}

ui big_integer::get_digit(size_t ind) const {
	if (ind < length()) {
		return data[ind];
	}
	else if (sign) {
		return MAX_DIGIT;
	}
	else {
		return 0;
	}
}


void big_integer::make_fit() {
	while (!data.empty() && ((sign && data.back() == MAX_DIGIT) || (!sign && data.back() == 0))) {
		data.pop_back();
	}
}

big_integer::big_integer(bool new_sign, ui_vector const &new_data) : sign(new_sign), data(new_data) {
	make_fit();
}

big_integer::big_integer() : sign(false) {}

big_integer::big_integer(big_integer const &other) : sign(other.sign), data(other.data) {
	make_fit();
}

big_integer::big_integer(int a) : sign(a < 0), data(1) {

	data[0] = my_ui_cast(a);
	make_fit();
}

big_integer::big_integer(ui a) : sign(0), data(1) {
	data[0] = a;
	make_fit();
}

big_integer::big_integer(ull a) : sign(0), data(2) {
	data[0] = my_ui_cast(a);
	data[1] = my_ui_cast(a >> BASE);
	make_fit();
}

big_integer &big_integer::operator=(big_integer const &other) {
	big_integer temp(other);
	swap(temp);
	return *this;
}

big_integer &big_integer::operator+=(big_integer const &rhs) {
	return *this = *this + rhs;
}

big_integer &big_integer::operator-=(big_integer const &rhs) {
	return *this = *this - rhs;
}

big_integer &big_integer::operator*=(big_integer const &rhs) {
	return *this = *this * rhs;
}

big_integer& big_integer::operator/=(big_integer const& rhs)
{
	return *this = *this / rhs;
}

big_integer& big_integer::operator%=(big_integer const& rhs)
{
	return *this = *this % rhs;
}

big_integer& big_integer::operator&=(big_integer const &rhs) {
	return *this = *this & rhs;
}

big_integer& big_integer::operator|=(big_integer const &rhs) {
	return *this = *this | rhs;
}

big_integer& big_integer::operator^=(big_integer const &rhs) {
	return *this = *this ^ rhs;
}

big_integer& big_integer::operator<<=(ui rhs) {
	return *this = *this << rhs;
}

big_integer& big_integer::operator>>=(ui rhs) {
	return *this = *this >> rhs;
}

big_integer big_integer::operator+() const {
	return *this;
}

big_integer big_integer::operator-() const {
	if (is_zero()) {
		return *this;
	}
	else if (length() == 0) {
		return big_integer(1u);
	}
	size_t n = length() + 2;
	ui_vector temp(n);
	ull sum = my_ui_cast(~get_digit(0)) + 1ULL, carry = sum >> BASE;
	temp[0] = my_ui_cast(sum);
	for (size_t i = 1; i < n - 2; i++) {
		sum = carry + my_ull_cast(~get_real_digit(i));
		temp[i] = my_ui_cast(sum);
		carry = sum >> BASE;
	}
	for (size_t i = n - 2; i < n; i++) {
		sum = (carry + (sign ? 0 : MAX_DIGIT));
		temp[i] = my_ui_cast(sum);
		carry = sum >> BASE;
	}
	return big_integer(temp.back() & (1 << (BASE - 1)), temp);
}

big_integer big_integer::operator~() const {
	ui_vector temp(data);
	for (size_t i = 0; i < data.size(); i++) {
		temp[i] = ~data[i];
	}
	return big_integer(!sign, temp);
}

big_integer& big_integer::operator++() {
	*this += 1;
	return *this;
}

big_integer big_integer::operator++(int) {
	big_integer temp(*this);
	++(*this);
	return temp;
}

big_integer& big_integer::operator--() {
	*this -= 1;
	return *this;
}

big_integer big_integer::operator--(int) {
	big_integer temp(*this);
	--(*this);
	return temp;
}

big_integer operator&(big_integer const &a, big_integer const &b) {
	size_t n = std::max(a.length(), b.length()), m = std::min(a.length(), b.length());
	ui_vector temp(n);
	for (size_t i = 0; i < m; i++) {
		temp[i] = a.get_real_digit(i) & b.get_real_digit(i);
	}
	for (size_t i = m; i < n; i++) {
		temp[i] = a.get_digit(i) & b.get_digit(i);
	}
	return big_integer(a.sign & b.sign, temp);
}

big_integer operator|(big_integer const &a, big_integer const &b) {
	size_t n = std::max(a.length(), b.length()), m = std::min(a.length(), b.length());
	ui_vector temp(n);
	for (size_t i = 0; i < m; i++) {
		temp[i] = a.get_real_digit(i) | b.get_real_digit(i);
	}
	for (size_t i = m; i < n; i++) {
		temp[i] = a.get_digit(i) | b.get_digit(i);
	}
	return big_integer(a.sign | b.sign, temp);
}

big_integer operator^(big_integer const &a, big_integer const &b) {
	size_t n = std::max(a.length(), b.length()), m = std::min(a.length(), b.length());
	ui_vector temp(n);
	for (size_t i = 0; i < m; i++) {
		temp[i] = a.get_real_digit(i) ^ b.get_real_digit(i);
	}
	for (size_t i = m; i < n; i++) {
		temp[i] = a.get_digit(i) ^ b.get_digit(i);
	}
	return big_integer(a.sign ^ b.sign, temp);
}

big_integer operator<<(big_integer const &a, ui b) {
	if (b == 0) {
		return big_integer(a);
	}
	size_t div = b >> 5;
	size_t mod = b & (BASE - 1);
	size_t new_size = a.length() + div + 1;
	ui_vector temp(new_size);
	temp[div] = my_ui_cast(ull(a.get_digit(0)) << mod);
	for (size_t i = div + 1; i < new_size; i++) {
		ull x = ull(a.get_digit(i - div)) << mod;
		ull y = ull(a.get_real_digit(i - div - 1)) >> (BASE - mod);
		temp[i] = my_ui_cast(x | y);
	}
	return big_integer(a.sign, temp);
}

big_integer operator>>(big_integer const &a, ui b) {
	if (b == 0) {
		return big_integer(a);
	}
	size_t div = b >> 5;
	size_t mod = b & (BASE - 1);
	size_t new_size = 0;
	if (div < a.length()) {
		new_size = a.length() - div;
	}
	ui_vector temp(new_size);
	for (size_t i = 0; i < new_size; i++) {
		ull x = ull(a.get_real_digit(i + div)) >> mod;
		ull y = ull(a.get_digit(i + div + 1)) << (BASE - mod);
		temp[i] = my_ui_cast(x | y);
	}
	return big_integer(a.sign, temp);
}

bool operator==(big_integer const &a, big_integer const &b) {
	return (a.sign == b.sign) && (a.data == b.data);
}

bool operator!=(big_integer const &a, big_integer const &b) {
	return !(a == b);
}

bool operator<(big_integer const &a, big_integer const &b) {
	if (a.sign != b.sign) {
		return a.sign;
	}
	if (a.length() != b.length()) {
		return a.length() < b.length();
	}

	for (size_t i = a.length(); i > 0; i--) {
		if (a.get_digit(i - 1) != b.get_digit(i - 1)) {
			return a.get_real_digit(i - 1) < b.get_real_digit(i - 1);
		}
	}
	return 0;
}

bool operator>(big_integer const &a, big_integer const &b) {
	return b < a;
}

bool operator<=(big_integer const &a, big_integer const &b) {
	return !(a > b);
}

bool operator>=(big_integer const &a, big_integer const &b) {
	return !(a < b);
}

std::string to_string(big_integer const& a)
{
	if (a.is_zero()) {
		return "0";
	}
	else if (a.length() == 0) {
		return "-1";
	}
	std::string ans = "";
	big_integer abs_a(a.abs());
	while (!abs_a.is_zero())
	{
		ui temp = (abs_a % BASE_10).get_digit(0);
		for (size_t i = 0; i < 9; i++) {
			ans.push_back('0' + temp % 10);
			temp /= 10;
		}
		abs_a /= BASE_10;
	}
	while (!ans.empty() && ans.back() == '0')
	{
		ans.pop_back();
	}
	if (a.sign) {
		ans.push_back('-');
	}
	reverse(ans.begin(), ans.end());
	return ans;
}

big_integer operator+(big_integer const &a, big_integer const &b) {
	size_t n = std::max(a.length(), b.length()) + 2, m = std::min(a.length(), b.length());
	ui_vector temp(n);
	ull carry = 0, sum = 0;
	for (size_t i = 0; i < m; i++) {
		sum = (carry + a.get_real_digit(i)) + b.get_real_digit(i);
		temp[i] = my_ui_cast(sum);
		carry = sum >> BASE;
	}
	for (size_t i = m; i < n; i++) {
		sum = (carry + a.get_digit(i)) + b.get_digit(i);
		temp[i] = my_ui_cast(sum);
		carry = sum >> BASE;
	}
	return big_integer(temp.back() & (1 << (BASE - 1)), temp);
}

big_integer operator-(big_integer const &a, big_integer const &b) {
	size_t n = std::max(a.length(), b.length()) + 3, m = std::min(a.length(), b.length());
	ui_vector temp(n);
	ull carry = 0, sum = 0;
	if (m > 0) {
		sum = my_ull_cast(a.get_real_digit(0)) + 1ULL + my_ull_cast(~b.get_real_digit(0));
		temp[0] = my_ui_cast(sum);
		carry = sum >> BASE;
		for (size_t i = 1; i < m; i++) {
			sum = carry + my_ull_cast(a.get_real_digit(i)) + my_ull_cast(~b.get_real_digit(i));
			temp[i] = my_ui_cast(sum);
			carry = sum >> BASE;
		}
		for (size_t i = m; i < n; i++) {
			sum = carry + my_ull_cast(a.get_digit(i)) + my_ull_cast(~b.get_digit(i));
			temp[i] = my_ui_cast(sum);
			carry = sum >> BASE;
		}
	}
	else {
		sum = my_ull_cast(a.get_digit(0)) + 1ULL + my_ull_cast(~b.get_digit(0));
		temp[0] = my_ui_cast(sum);
		carry = sum >> BASE;
		for (size_t i = 1; i < n; i++) {
			sum = carry + my_ull_cast(a.get_digit(i)) + my_ull_cast(~b.get_digit(i));
			temp[i] = my_ui_cast(sum);
			carry = sum >> BASE;
		}
	}
	return big_integer(temp.back() & (1 << (BASE - 1)), temp);

}

void mul_vector(ui_vector &res, ui_vector const &a, ui_vector const &b) {
	size_t n = a.size(), m = b.size();
	for (size_t i = 0; i < n; i++) {
		ull carry = 0, mul = 0, tmp = 0;
		for (size_t j = 0; j < m; j++) {
			size_t k = i + j;
			mul = ull(a[i]) * b[j];
			tmp = (mul & MAX_DIGIT) + res[k] + carry;
			res[k] = my_ui_cast(tmp);
			carry = (mul >> BASE) + (tmp >> BASE);
		}
		res[i + m] += my_ui_cast(carry);
	}
}

void mul_big_small(ui_vector &res, ui_vector const &a, const ui b) {
	size_t n = a.size();
	res.resize(n + 1);
	ull carry = 0, mul = 0, tmp = 0;
	for (size_t i = 0; i < n; i++) {
		mul = ull(a[i]) * b;
		tmp = (mul & MAX_DIGIT) + carry;
		res[i] = my_ui_cast(tmp);
		carry = (mul >> BASE) + (tmp >> BASE);
	}
	res[n] = my_ui_cast(carry);
}

void big_integer::correct() {
	if (!sign) {
		return;
	}
	else if (length() == 0) {
		sign = !sign;
		return;
	}
	data.prepare_for_change();
	size_t n = length();
	ull sum = my_ull_cast(~data[0]) + 1ULL, carry = sum >> BASE;
	data[0] = my_ui_cast(sum);
	for (size_t i = 1; i < n; i++) {
		sum = carry + my_ull_cast(~data[i]);
		data[i] = my_ui_cast(sum);
		carry = sum >> BASE;
	}
	data.push_back(my_ui_cast(carry + MAX_DIGIT));
	make_fit();
}

big_integer operator*(big_integer const &a, big_integer const &b) {
	if (a.is_zero() || b.is_zero()) {
		return big_integer(0u);
	}
	big_integer abs_a(a.abs());
	big_integer abs_b(b.abs());
	if (abs_a.length() > abs_b.length()) {
		abs_a.swap(abs_b);
	}
	size_t n = abs_a.length(), m = abs_b.length();
	size_t len = (n + m + 1);
	ui_vector temp(len);
	if (abs_a.length() == 1) {
		mul_big_small(temp, abs_b.data, abs_a.get_real_digit(0));
	}
	else {
		mul_vector(temp, abs_a.data, abs_b.data);
	}
	big_integer res(a.sign ^ b.sign, temp);
	res.correct();
	return res;
}

int string_to_int(std::string const &s) {
	int ans = 0;
	for (auto a : s) {
		if (a < '0' || a > '9') {
			throw std::runtime_error("Incorrect char");
		}
		ans = ans * 10 + (a - '0');
	}
	return ans;
}

int dec_pow(ui st) {
	if (st == 0) {
		return 1;
	}
	if (st & 1) {
		return dec_pow(st - 1) * 10;
	}
	int t = dec_pow(st >> 1);
	return t * t;
}

big_integer string_to_number(std::string const &s) {
	big_integer res(0);
	size_t beg = 0;
	if (s[beg] == '-') {
		beg++;
	}
	for (size_t i = beg; i + 9 <= s.length(); i += 9) {
		res = res * BASE_10 + string_to_int(s.substr(i, 9));
	}
	ui mod = (s.length() - beg) % 9;
	if (mod > 0) {
		res = res * dec_pow(mod) + string_to_int(s.substr(s.length() - mod, mod));
	}
	return beg > 0 ? -res : res;
}

big_integer::big_integer(std::string const &str) :big_integer(string_to_number(str)) {}

ui get_trial(const ui a, const ui b, const ui c) {
	ull res = a;
	res = ((res << BASE) + b) / c;
	if (res > MAX_DIGIT) {
		res = MAX_DIGIT;
	}
	return my_ui_cast(res);
}

void sub_equal_vectors(ui_vector &a, ui_vector const &b) {
	ull sum = my_ull_cast(~b[0]) + my_ull_cast(a[0]) + 1LL, carry = sum >> BASE;
	a[0] = my_ui_cast(sum);
	for (size_t i = 1; i < b.size(); i++) {
		sum = my_ull_cast(~b[i]) + my_ull_cast(a[i]) + carry;
		a[i] = my_ui_cast(sum);
		carry = sum >> BASE;
	}
}

bool compare_equal_vectors(ui_vector const &a, ui_vector const &b) {
	for (size_t i = a.size(); i > 0; i--) {
		if (a[i - 1] != b[i - 1]) {
			return a[i - 1] < b[i - 1];
		}
	}
	return 0;
}

big_integer operator/(big_integer const &a, big_integer const &b) {
	if (b.is_zero()) {
		throw std::runtime_error("Divison by zero");
	}
	big_integer abs_a(a.abs());
	big_integer abs_b(b.abs());
	if (abs_a < abs_b) {
		return 0;
	}

	const ui f = my_ui_cast((ull(MAX_DIGIT) + 1) / (ull(abs_b.data.back()) + 1));
	const size_t n = abs_a.length(), m = abs_b.length();
	abs_a.data.prepare_for_change();
	abs_b.data.prepare_for_change();
	mul_big_small(abs_a.data, abs_a.data, f);
	mul_big_small(abs_b.data, abs_b.data, f);
	abs_a.make_fit();
	abs_b.make_fit();

	const size_t len = n - m + 1;
	const ui divisor = abs_b.data.back();
	ui_vector temp(len);
	ui_vector dev(m + 1), div(m + 1);
	for (size_t i = 0; i < m; i++) {
		dev[i] = abs_a.get_real_digit(n + i - m);
	}
	dev[m] = abs_a.get_digit(n);
	if (abs_b.length() == 1) {
		for (size_t i = 0; i < len; i++) {
			dev[0] = abs_a.get_real_digit(n - m - i);
			size_t ri = len - 1 - i;
			ui tq = get_trial(dev[m], dev[m - 1], divisor);
			mul_big_small(div, abs_b.data, tq);
			sub_equal_vectors(dev, div);
			for (size_t j = m; j > 0; j--) {
				dev[j] = dev[j - 1];
			}
			temp[ri] = tq;
		}
	}
	else {
		for (size_t i = 0; i < len; i++) {
			dev[0] = abs_a.get_real_digit(n - m - i);
			size_t ri = len - 1 - i;
			ui tq = get_trial(dev[m], dev[m - 1], divisor);
			mul_big_small(div, abs_b.data, tq);
			while ((tq >= 0) && compare_equal_vectors(dev, div)) {
				mul_big_small(div, abs_b.data, --tq);
			}
			sub_equal_vectors(dev, div);
			for (size_t j = m; j > 0; j--) {
				dev[j] = dev[j - 1];
			}
			temp[ri] = tq;
		}
	}
	big_integer res(a.sign ^ b.sign, temp);
	res.correct();
	return res;
}

big_integer operator%(big_integer const &a, big_integer const& b) {
	return a - (a / b) * b;
}