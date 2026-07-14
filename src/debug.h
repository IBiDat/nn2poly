#ifndef nn2poly__debug_h
#define nn2poly__debug_h

namespace nn2poly {
namespace detail {

#ifdef NN2POLY_DEBUG

#define NN2POLY_DEFINE_DEBUG_VAR(env_name, var_name, default_value) \
  inline const int var_name = []() { \
    if (const char* i = std::getenv(#env_name)) \
      return std::atoi(i); \
    return default_value; \
  }();
NN2POLY_DEFINE_DEBUG_VAR(NN2POLY_DEBUG_LEVEL, debug_level, 0)
NN2POLY_DEFINE_DEBUG_VAR(NN2POLY_DEBUG_VECTOR_DEPTH, debug_vector_depth, 10)

#define DTAG(x) " " #x "=", x
#define NN2POLY_DEBUG_LOG(level, ...) \
  do { \
    if ((level) <= ::nn2poly::detail::debug_level) { \
      ::nn2poly::detail::debug_log("[nn2poly][DEBUG", (level), "][", \
        __FILE__, ":", __LINE__, "][", __func__, "]", \
        __VA_ARGS__); \
    } \
  } while(0)

// Helper
template <typename T>
struct Printable {
  const T& val;
  size_t indent = 0;
};

template <typename... Args>
void debug_log(Args&&... args) {
  (std::cerr << ... << Printable<std::decay_t<Args>>{args}) << '\n';
}

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
template <typename T> inline constexpr size_t vector_depth_v = vector_depth<T>::value;

// Vectors and nested vectors
template <typename T>
std::ostream& operator<<(std::ostream& os, const Printable<std::vector<T>>& p) {
  if (static_cast<int>(vector_depth_v<T>) + 1 >= debug_vector_depth) {
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

} // namespace detail
} // namespace nn2poly

#define NN2POLY_DEFINE_PRINTABLE_STRUCT_2(Type, M1, M2) \
  inline std::ostream& operator<<(std::ostream& os, const Type& s) { \
    return os << "{" \
      << #M1 ": " << nn2poly::detail::Printable<decltype(s.M1)>{s.M1, 0} << ", " \
      << #M2 ": " << nn2poly::detail::Printable<decltype(s.M2)>{s.M2, 0} \
      << "}"; \
  }
NN2POLY_DEFINE_PRINTABLE_STRUCT_2(TermSummary, unique_terms, counts)
NN2POLY_DEFINE_PRINTABLE_STRUCT_2(TermEquivalence, signature, canonical_order)

namespace nn2poly {
namespace detail {

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
    if (it != this->end()) { hits++; } else { misses++; }
    return it;
  }

  // Shadow the const find
  typename Base::const_iterator find(const Key& key) const {
    auto it = Base::find(key);
    if (it != this->end()) { hits++; } else { misses++; }
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

} // namespace detail

struct PartitionCache {
  detail::unordered_map<Term, Partition, TermHash> signature;
  detail::unordered_map<TermQ, Partition, TermQHash> filtered;
  detail::unordered_map<TermQ, Partition, TermQHash> renamed;

#ifdef NN2POLY_DEBUG
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
