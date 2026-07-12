#ifndef nn2poly__debug_h
#define nn2poly__debug_h

#include <unordered_map>
#include <vector>
#include <iostream>
#include <string>

#ifndef NN2POLY_DEBUG
#define NN2POLY_DEBUG 0
#endif

#define DTAG(x) " " #x "=", x

namespace nn2poly {

#if NN2POLY_DEBUG >= 1

#define NN2POLY_DEBUG_LOG(level, ...) \
  do { \
    if constexpr ((level) <= NN2POLY_DEBUG) { \
      ::nn2poly::detail::log_debug( \
        "[nn2poly][DEBUG", (level), "][", __FILE__, ":", __LINE__, "]", \
        __VA_ARGS__); \
    } \
  } while(0)

namespace detail {

// Helper
template <typename T>
struct Printable {
  const T& val;
  size_t indent = 0;
};

template <typename, typename = void> struct is_mapping : std::false_type {};
template <typename T> struct is_mapping<T,
  std::void_t<typename T::key_type, typename T::mapped_type>> : std::true_type {};
template <typename T> inline constexpr bool is_mapping_v = is_mapping<T>::value;

// Standard types
template <typename T>
std::enable_if_t<!is_mapping_v<T>, std::ostream&>
operator<<(std::ostream& os, const Printable<T>& p) {
  return os << p.val;
}

// Maps
template <typename T>
std::enable_if_t<is_mapping_v<T>, std::ostream&>
operator<<(std::ostream& os, const Printable<T>& p) {
  os << "{";
  size_t count = 0;
  for (const auto& [key, value] : p.val) {
    using KeyT = std::decay_t<decltype(key)>;
    using ValT = std::decay_t<decltype(value)>;
    os << Printable<KeyT>{key, 0} << ": " << Printable<ValT>{value, p.indent};
    if (count != p.val.size() - 1) os << ", ";
    count++;
  }
  os << "}";
  return os;
}

template <typename T> struct vector_depth { static constexpr size_t value = 0; };
template <typename T, typename A> struct vector_depth<std::vector<T, A>> {
  static constexpr size_t value = 1 + vector_depth<T>::value; };
template <typename T> inline constexpr bool vector_depth_v = vector_depth<T>::value;

// Vectors and nested vectors
template <typename T>
std::ostream& operator<<(std::ostream& os, const Printable<std::vector<T>>& p) {
  if constexpr (vector_depth_v<T> > 1) {
    std::string current_indent(p.indent, ' ');
    std::string next_indent(p.indent + 2, ' ');
    os << "[\n";
    for (size_t i = 0; i < p.val.size(); ++i) {
      os << next_indent << Printable<T>{p.val[i], p.indent + 2};
      if (i != p.val.size() - 1) os << ",";
      os << "\n";
    }
    os << current_indent << "]";
  } else {
    os << "[";
    for (size_t i = 0; i < p.val.size(); ++i) {
      os << Printable<T>{p.val[i], 0};
      if (i != p.val.size() - 1) os << ", ";
    }
    os << "]";
  }
  return os;
}

template <typename... Args>
void log_debug(Args&&... args) {
  (std::cerr << ... << Printable<std::decay_t<Args>>{args}) << '\n';
}

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

  struct DebugProxy {
    size_t hits, misses;

    friend std::ostream& operator<<(std::ostream& os, const DebugProxy& proxy) {
      return os << "hit/miss=" << proxy.hits << "/" << proxy.misses;
    }
  };

  DebugProxy delta() const {
    DebugProxy snap{hits - last_hits, misses - last_misses};
    last_hits = hits;
    last_misses = misses;
    return snap;
  }

  DebugProxy total() const {
    return {hits, misses};
  }
};

} // namespace detail

template <
  typename Key, typename T,
  typename Hash = std::hash<Key>,
  typename KeyEqual = std::equal_to<Key>,
  typename Allocator = std::allocator<std::pair<const Key, T>>
>
using unordered_map = detail::debug_unordered_map<Key, T, Hash, KeyEqual, Allocator>;

#else

#define NN2POLY_DEBUG_LOG(level, ...) do {} while(0)

template <
  typename Key, typename T,
  typename Hash = std::hash<Key>,
  typename KeyEqual = std::equal_to<Key>,
  typename Allocator = std::allocator<std::pair<const Key, T>>
>
using unordered_map = std::unordered_map<Key, T, Hash, KeyEqual, Allocator>;

#endif

struct PartitionCache {
  unordered_map<Term, Partition, TermHash> signature;
  unordered_map<TermQ, Partition, TermQHash> filtered;
  unordered_map<TermQ, Partition, TermQHash> renamed;

#if NN2POLY_DEBUG >= 1
  struct DebugProxy {
    const PartitionCache* pcache;
    bool delta;

    friend std::ostream& operator<<(std::ostream& os, const DebugProxy& proxy) {
      os << "[cache]";
      if (proxy.delta) {
        os << "[delta]"
          << " renamed " << proxy.pcache->renamed.delta()
          << " filtered " << proxy.pcache->filtered.delta()
          << " signature " << proxy.pcache->signature.delta();
      } else {
        os << "[total]"
          << " renamed " << proxy.pcache->renamed.total()
          << " filtered " << proxy.pcache->filtered.total()
          << " signature " << proxy.pcache->signature.total();
      }
      return os;
    }
  };

  DebugProxy debug(bool delta = false) const {
    return {this, delta};
  }
#endif
};

} // namespace nn2poly

#endif