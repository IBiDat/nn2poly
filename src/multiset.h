#ifndef multiset_h
#define multiset_h

#include <iterator>
#include <vector>
#include <set>
#include <numeric>
#include <algorithm>

template <typename T>
using Partition = std::vector<std::vector<T>>;

template <typename T>
class MultisetPartitions {
public:
  MultisetPartitions(std::multiset<T>& mset) {
    if (mset.empty()) return;

    std::set<T> s(mset.begin(), mset.end());
    for (auto i: s) {
      comp.push_back(i);
      mult.push_back(mset.count(i));
    }

    int m = mult.size();
    int n = std::accumulate(mult.begin(), mult.end(), 0);

    c.resize(m * n + 1);
    u.resize(m * n + 1);
    v.resize(m * n + 1);
    f.resize(n + 1);

  //M1: // initialize
    for (int j = 0; j < m; j++) {
      c[j] = j;
      u[j] = mult[j];
      v[j] = mult[j];
    }
    f[0] = a = l = 0;
    f[1] = b = m;

    M2M3();
  }

  bool next() {
  M5: // decrease v
    j = b - 1;
    while (v[j] == 0)
      j--;
    if (j == a && v[j] == 1)
      goto M6;
    v[j]--;
    for (k = j + 1; k < b; k++)
      v[k] = u[k];
    M2M3();
    return false; // goto M4

  M6: // backtrack
    if (l == 0)
      return true; // finished
    l--;
    b = a;
    a = f[l];
    goto M5;
  }

  Partition<T> get() const {
    Partition<T> partition;
    for (int i = 0; i <= l; i++) {
      std::vector<T> part;
      for (int j = f[i]; j < f[i + 1]; j++) {
        for (int k = 0; k < v[j]; k++)
          part.push_back(comp[c[j]]);
      }
      partition.push_back(part);
    }
    return partition;
  }

private:
  std::vector<T> comp;
  std::vector<int> mult;
  std::vector<int> c, u, v, f;
  int a, b, l, j, k;
  bool x;

  void M2M3() {
    while (true) {
    //M2: // subtract v from u
      x = false;
      k = b;
      for (j = a; j < b; j++) {
        u[k] = u[j] - v[j];
        if (u[k] == 0)
          x = true;
        else if (!x) {
          c[k] = c[j];
          v[k] = std::min(v[j], u[k]);
          x = u[k] < v[j];
          k++;
        } else {
          c[k] = c[j];
          v[k] = u[k];
          k++;
        }
      }

    //M3: // push if nonzero
      if (k > b) {
        a = b;
        b = k;
        l++;
        f[l + 1] = b;
        // goto M2;
      } else break;
    }
  }
};

template <typename T>
class multiset_partitions;

template <typename T>
bool operator==(const multiset_partitions<T>& lhs,
                const multiset_partitions<T>& rhs) {
  return lhs.done == rhs.done;
}
template <typename T>
bool operator!=(const multiset_partitions<T>& lhs,
                const multiset_partitions<T>& rhs) {
  return !(lhs == rhs);
}

template <typename T>
class multiset_partitions {
public:
  using iterator_category = std::input_iterator_tag;
  using value_type = MultisetPartitions<T>;
  using difference_type = ptrdiff_t;
  using pointer = value_type const*;
  using reference = value_type const&;

  multiset_partitions(std::multiset<T>& mset): done(mset.empty()), s(mset) {}

  explicit operator bool() const { return !done; }
  reference operator*() const { return s; }
  pointer operator->() const { return &s; }

  multiset_partitions<T>& operator++() {
    done = s.next();
    return *this;
  }

  multiset_partitions<T> operator++(int) {
    auto it = *this;
    ++*this;
    return it;
  }

  multiset_partitions<T>& begin() { return *this; }
  multiset_partitions<T>& end() { done = true; return *this; }
  friend bool operator==<>(const multiset_partitions<T>& lhs,
                           const multiset_partitions<T>& rhs);

private:
  bool done;
  MultisetPartitions<T> s;
};

#endif
