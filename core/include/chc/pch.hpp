#pragma once

#include <cmath>
#include <chrono>
#include <cassert>
#include <iostream>
#include <string>
#include <vector>
#include <deque>
#include <map>
#include <set>
#include <unordered_map>
#include <stack>
#include <variant>
#include <memory>
#include <functional>
#include <algorithm>
#include <numeric>
#include <optional>
#include <thread>
#include <future>
#include <mutex>
#include <condition_variable>
#include <type_traits>
#include <filesystem>
#include <fstream>

using u8 = unsigned char;
using u16 = unsigned short;
using u32 = unsigned int;
using u64 = unsigned long long;

using i8 = signed char;
using i16 = signed short;
using i32 = signed int;
using i64 = signed long long;

using f32 = float;
using f64 = double;

using String = std::string;
template<typename T>
using Opt = std::optional<T>;
template<typename T>
using Sptr = std::shared_ptr<T>;
using size_t = std::size_t;

using std::stod;
using std::stof;
using std::stoi;
using std::stol;
using std::stold;
using std::stoll;
using std::stoul;
using std::stoull;
using std::to_string;

// Print a message
void olog( const String &str );
// Log a debug message
void log( const String &str );

#define chc_VERSION "0.1"
