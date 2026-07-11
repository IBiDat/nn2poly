#ifndef nn2poly__debug_h
#define nn2poly__debug_h

#include <unordered_map>
#include <iostream>
#include <string>

#ifdef NN2POLY_DEBUG

template <
  typename Key,
  typename T,
  typename Hash = std::hash<Key>,
  typename KeyEqual = std::equal_to<Key>,
  typename Allocator = std::allocator<std::pair<const Key, T>>
>
class debug_unordered_map : public std::unordered_map<Key, T, Hash, KeyEqual, Allocator> {
  using Base = std::unordered_map<Key, T, Hash, KeyEqual, Allocator>;

public:
  // Inherit all standard constructors
  using Base::Base;

  // Use mutable so we can track hits/misses even in const maps or const methods
  mutable size_t hits = 0;
  mutable size_t misses = 0;

  // Track state for delta calculations
  mutable size_t last_hits = 0;
  mutable size_t last_misses = 0;

  // Shadow the non-const find
  typename Base::iterator find(const Key& key) {
    auto it = Base::find(key);
    if (it != this->end()) {
      hits++;
    } else {
      misses++;
    }
    return it;
  }

  // Shadow the const find
  typename Base::const_iterator find(const Key& key) const {
    auto it = Base::find(key);
    if (it != this->end()) {
      hits++;
    } else {
      misses++;
    }
    return it;
  }

#if __cplusplus >= 202002L
  template<class K>
  typename Base::iterator find(const K& x) {
    auto it = Base::find(x);
    if (it != this->end()) { hits++; } else { misses++; }
    return it;
  }

  template<class K>
  typename Base::const_iterator find(const K& x) const {
    auto it = Base::find(x);
    if (it != this->end()) { hits++; } else { misses++; }
    return it;
  }
#endif

  struct DeltaProxy {
    const debug_unordered_map* map;

    friend std::ostream& operator<<(std::ostream& os, const DeltaProxy& proxy) {
      os << "hit/miss=" << proxy.map->hits - proxy.map->last_hits
         << "/"         << proxy.map->misses - proxy.map->last_misses;
      proxy.map->last_hits = proxy.map->hits;
      proxy.map->last_misses = proxy.map->misses;
      return os;
    }
  };

  struct TotalProxy {
    const debug_unordered_map* map;

    friend std::ostream& operator<<(std::ostream& os, const TotalProxy& proxy) {
      os << "hit/miss=" << proxy.map->hits
         << "/"         << proxy.map->misses;
      return os;
    }
  };

  DeltaProxy delta() const { return {this}; }
  TotalProxy total() const { return {this}; }
};

template <
  typename Key, typename T,
  typename Hash = std::hash<Key>,
  typename KeyEqual = std::equal_to<Key>,
  typename Allocator = std::allocator<std::pair<const Key, T>>
>
using unordered_map = debug_unordered_map<Key, T, Hash, KeyEqual, Allocator>;

#else

template <
  typename Key, typename T,
  typename Hash = std::hash<Key>,
  typename KeyEqual = std::equal_to<Key>,
  typename Allocator = std::allocator<std::pair<const Key, T>>
>
using unordered_map = std::unordered_map<Key, T, Hash, KeyEqual, Allocator>;

#endif

#endif