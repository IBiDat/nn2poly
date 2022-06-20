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
class multiset_partitions;

template <typename T>
class PState {
  friend class multiset_partitions<T>;
public:
  PState(std::multiset<T>& mset) {
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
  int a, b, l;
};

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
  using value_type = PState<T>;
  using difference_type = ptrdiff_t;
  using pointer = value_type const*;
  using reference = value_type const&;

  multiset_partitions(std::multiset<T>& mset): done(false), s(mset) {
    if (mset.empty())
      done = true;
    else M2M3();
  }

  explicit operator bool() const { return !done; }
  auto operator*() const { return s; }
  auto operator->() const { return &s; }

  auto operator++() {
  M5: // decrease v
    j = s.b - 1;
    while (s.v[j] == 0)
      j--;
    if (j == s.a && s.v[j] == 1)
      goto M6;
    s.v[j]--;
    for (k = j + 1; k < s.b; k++)
      s.v[k] = s.u[k];
    M2M3();
    goto M4;

  M6: // backtrack
    if (s.l == 0) {
      done = true;
      goto M4;
    }
    s.l--;
    s.b = s.a;
    s.a = s.f[s.l];
    goto M5;

  M4: // visit a partition
    return *this;
  }

  auto operator++(int) {
    auto it = *this;
    ++*this;
    return it;
  }

  auto begin() const { return *this; }
  auto end() { done = true; return *this; }
  friend bool operator==<>(const multiset_partitions<T>& lhs,
                           const multiset_partitions<T>& rhs);

private:
  bool done;
  PState<T> s;
  int j, k;
  bool x;

  void M2M3() {
    while (true) {
    //M2: // subtract v from u
      x = false;
      k = s.b;
      for (j = s.a; j < s.b; j++) {
        s.u[k] = s.u[j] - s.v[j];
        if (s.u[k] == 0)
          x = true;
        else if (!x) {
          s.c[k] = s.c[j];
          s.v[k] = std::min(s.v[j], s.u[k]);
          x = s.u[k] < s.v[j];
          k++;
        } else {
          s.c[k] = s.c[j];
          s.v[k] = s.u[k];
          k++;
        }
      }

    //M3: // push if nonzero
      if (k > s.b) {
        s.a = s.b;
        s.b = k;
        s.l++;
        s.f[s.l + 1] = s.b;
        // goto M2;
      } else break;
    }
  }
};

#endif
