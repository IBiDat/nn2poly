#ifndef nn2poly__multiset_h
#define nn2poly__multiset_h

#include <iterator>
#include <vector>
#include <set>
#include <numeric>
#include <algorithm>

template <typename T>
using Partition = std::vector<std::vector<T>>;

template <typename T>
class MultisetPartitions {
  std::vector<T> comp;
  std::vector<int> mult;

public:
  using value_type = Partition<T>;
  using pointer = value_type*;
  using reference = value_type&;

  MultisetPartitions(std::multiset<T>& mset) {
    for (auto i: std::set<T>(mset.begin(), mset.end())) {
      comp.push_back(i);
      mult.push_back(mset.count(i));
    }
  }

  struct iterator;
  iterator begin() { return iterator(this); }
  iterator end() { return iterator(NULL); }

  struct iterator {
    using iterator_category = std::input_iterator_tag;
    using value_type = MultisetPartitions<T>::value_type;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    iterator(MultisetPartitions<T>* obj): obj(obj) {
      if (!(done = !obj || obj->comp.empty()))
        init();
    }

    explicit operator bool() const { return !done; }
    value_type operator*() { return get(); }
    pointer operator->() const { return &get(); }

    iterator& operator++() { done = next(); return *this; }
    iterator operator++(int) { auto it = *this; ++*this; return it; }

    friend bool operator==(const iterator& lhs, const iterator& rhs) {
      return lhs.done == rhs.done;
    };
    friend bool operator!=(const iterator& lhs, const iterator& rhs) {
      return lhs.done != rhs.done;
    };

  private:
    MultisetPartitions<T>* obj;
    std::vector<int> c, u, v, f;
    int a, b, l, j, k;
    bool x, done;

    void init() {
      int m = obj->mult.size();
      int n = std::accumulate(obj->mult.begin(), obj->mult.end(), 0);

      c.resize(m * n + 1);
      u.resize(m * n + 1);
      v.resize(m * n + 1);
      f.resize(n + 1);

    //M1: // initialize
      for (int j = 0; j < m; j++) {
        c[j] = j;
        u[j] = obj->mult[j];
        v[j] = obj->mult[j];
      }
      f[0] = a = l = 0;
      f[1] = b = m;

      main();
    }

    void main() {
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

      //M4: // visit partition
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
      main();
      return false; // goto M4

    M6: // backtrack
      if (l == 0)
        return true; // finished
      l--;
      b = a;
      a = f[l];
      goto M5;
    }

    value_type get() {
      value_type partition;
      for (int i = 0; i <= l; i++) {
        std::vector<T> part;
        for (int j = f[i]; j < f[i + 1]; j++) {
          for (int k = 0; k < v[j]; k++)
            part.push_back(obj->comp[c[j]]);
          Rcpp::checkUserInterrupt();
        }
        partition.push_back(part);
      }
      return partition;
    }
  };
};

template <typename T>
MultisetPartitions<T> multiset_partitions(std::multiset<T>& mset) {
  return MultisetPartitions<T>(mset);
}

#endif
