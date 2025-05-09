#pragma once
#include "pch.hpp"

namespace chc {

/// A generic iterator class for the compiler.
template <typename T>
class LazyIterator {
    struct SharedState {
        std::deque<Opt<T>> cache;
        std::function<Opt<T>()> gen;
        void gen_more() {
            if ( !gen ) {
                if ( cache.empty() )
                    cache.push_back( {} );
            } else {
                cache.push_back( gen() );
            }
        }
    };
    std::shared_ptr<SharedState> state;
    size_t idx = 0;
    ssize_t limit = INTPTR_MAX;
    Opt<T> none = {};

public:
    LazyIterator( std::function<Opt<T>()> generator ) {
        state = std::make_shared<SharedState>();
        state->gen = generator;
    }
    LazyIterator( const LazyIterator<T> &other ) {
        state = other.state;
        idx = other.idx;
        limit = other.limit;
    }
    /// Creates a iterator over an empty list.
    LazyIterator() { state = std::make_shared<SharedState>(); }

    bool curr_valid() const { return curr().has_value(); }
    bool curr_not_valid() const { return !curr().has_value(); }
    const Opt<T> &curr() const {
        if ( idx >= limit )
            return none; // Upper limit of elements reached.
        while ( state->cache.size() <= idx &&
                ( state->cache.empty() || state->cache.back().has_value() ) )
            state->gen_more(); // Generate new elements (except there are none
                               // left).
        if ( idx >= state->cache.size() - 1 &&
             !state->cache.back().has_value() ) {
            // Reached end of data stream (even if index is bigger).
            return none;
        }
        return state->cache.at( idx );
    }
    /// Unsafe variant of curr()
    const T &get() const { return *curr(); }
    /// Safe version of get() which returns an alternative, if the element would
    /// be None.
    const T &get_or( const T &alternative ) const {
        const auto &ret = curr();
        if ( ret.has_value() ) {
            return *ret;
        } else {
            return alternative;
        }
    }

    /// Does both get the value and increment the iterator.
    const T &consume() {
        auto &ret = curr();
        skip_self( 1 );
        return *ret;
    }

    /// Is for consume(), what get_or() is for get().
    const T &consume_or( T &alternative ) {
        auto &ret = curr();
        skip_self( 1 );
        if ( ret.has_value() ) {
            return *ret;
        } else {
            return alternative;
        }
    }

    /// Returns a new iterator with the specified index applied.
    LazyIterator<T> skip( ssize_t count = 1 ) const {
        LazyIterator ret( *this );
        ret.skip_self( count );
        return ret;
    }
    /// Jumps given elements forward/backward.
    LazyIterator<T> &skip_self( ssize_t count = 1 ) {
        if ( count < 0 && -count > idx ) {
            idx = 0; // Don't underflow.
        } else {
            idx += count;
        }
        return *this;
    }

    /// Returns an iterator which represents up to \param count elements of this
    /// iterator.
    LazyIterator<T> take( size_t count ) const {
        LazyIterator<T> ret = *this;
        ret.limit = std::min<ssize_t>( limit, idx + count );
        return ret;
    }

    /// Returns an iterator which represents all elements until the first one
    /// for which \param predicate is not fulfilled.
    LazyIterator<T> take_until(
        std::function<bool( const T & )> predicate ) const {
        return split( predicate ).first;
    }

    /// Returns an iterator which represents all elements not taken by
    /// take_until(predicate).
    LazyIterator<T> skip_until(
        std::function<bool( const T & )> predicate ) const {
        return split( predicate ).second;
    }

    /// Returns two iterators separating the elements into two lists, split at
    /// the first element, which fulfills \param predicate. In other
    /// words, a pair with (take_until(), skip_until()).
    std::pair<LazyIterator<T>, LazyIterator<T>> split(
        std::function<bool( const T & )> predicate ) const {
        auto itr = *this;
        size_t counter = 0;
        while ( true ) {
            auto &next = itr.curr();
            if ( !next.has_value() || predicate( *next ) )
                break;
            itr.skip_self( 1 );
            counter++;
        }
        return std::make_pair( take( counter ), skip( counter ) );
    }

    /// Splits the elements whenever one fulfills \param predicate. Returns an
    /// iterator of iterators to the separated lists. The first element is never
    /// empty due to splitting.
    LazyIterator<LazyIterator<T>> split_all(
        std::function<bool( const T & )> predicate ) const {
        auto itr = std::make_shared<LazyIterator<T>>( *this );
        return LazyIterator<LazyIterator<T>>(
            [itr, predicate]() -> Opt<LazyIterator<T>> {
                if ( itr->curr_not_valid() )
                    return {}; // Last element
                if ( predicate( itr->get() ) ) {
                    // First element should not be splitting point.
                    auto pair = itr->skip( 1 ).split( predicate );
                    *itr = pair.second;
                    return pair.first - 1;
                } else {
                    auto pair = itr->split( predicate );
                    *itr = pair.second;
                    return pair.first;
                }
            } );
    }

    /// Returns the last element of this iterator's elements.
    const Opt<T> &last() const {
        auto itr = end();
        --itr;
        return itr.curr();
    }

    /// Returns an iterator representing the position after last element of this
    /// iterator's elements.
    LazyIterator<T> end() const {
        auto itr = *this;
        while ( itr.curr_valid() )
            itr.skip_self( 1 );
        return itr;
    }

    /// Eager evaluation of all elements using \param func.
    void for_each( std::function<void( const T & )> func ) const {
        auto itr = *this;
        while ( itr.curr_valid() )
            func( itr.consume() );
    }


    /// Maps all elements to new elements.
    template <typename U>
    LazyIterator<U> map( std::function<U( const T & )> func ) const {
        auto itr = std::make_shared<LazyIterator<T>>( *this );
        return LazyIterator<U>( [itr, func]() -> Opt<U> {
            auto &next = itr->curr();
            if ( !next.has_value() )
                return {};
            itr->skip_self( 1 );
            return func( *next );
        } );
    }

    /// Returns an iterator which represents the elements of this iterator,
    /// which fulfill \param predicate.
    LazyIterator<T> filter( std::function<bool( const T & )> predicate ) const {
        auto itr = std::make_shared<LazyIterator<T>>( *this );
        return LazyIterator<T>( [itr, predicate]() -> Opt<T> {
            while ( true ) {
                auto &next = itr->curr();
                if ( !next.has_value() )
                    return {};
                itr->skip_self( 1 );
                if ( predicate( *next ) )
                    return *next;
            }
        } );
    }

    /// Returns an iterator which maps elements of this iterator to new elements
    /// and filters out elements for which \param func returns None.
    template <typename U>
    LazyIterator<U> map_filter(
        std::function<Opt<U>( const T & )> func ) const {
        auto itr = std::make_shared<LazyIterator<T>>( *this );
        return LazyIterator<U>( [itr, func]() -> Opt<U> {
            while ( true ) {
                auto &next = itr->curr();
                if ( !next.has_value() )
                    return {};
                itr->skip_self( 1 );
                auto candidate = func( *next );
                if ( candidate.has_value() )
                    return candidate;
            }
        } );
    }

    /// Reduces all elements of this iterator to a single value by accumulating
    /// them using \param func starting with \param init.
    template <typename U>
    U fold( std::function<U( const T &, const U & )> func,
            const U &init ) const {
        auto acc = init;
        auto itr = *this;
        while ( itr ) {
            acc = func( itr.consume(), acc );
        }
        return acc;
    }

    /// Reduces all elements of this iterator to a single value by accumulating
    /// them using \param func starting with \param init. \param func must
    /// combine T back into its parameter.
    template <typename U>
    U fold( std::function<void( const T &, U & )> func, const U &init ) const {
        auto acc = init;
        auto itr = *this;
        while ( itr ) {
            func( itr.consume(), acc );
        }
        return acc;
    }

    /// Returns the amount of elements of this iterator until the end of the
    /// list.
    size_t count() const {
        return fold<size_t>( []( const T &, size_t &i ) { ++i; }, 0 );
    }

    /// Concatenates all elements together using delimiter between them.
    T concat( const T &delimiter = T() ) const {
        T fallback;
        return skip( 1 ).template fold<T>(
            [delimiter]( const T &e, T &str ) { str += delimiter + e; },
            get_or( fallback ) );
    }

    /// Zips this iterator's elements together with another iterator's elements.
    template <typename U>
    LazyIterator<U> zip_with( std::function<U( const T &, const T & )> zipper,
                              LazyIterator<T> &other ) const {
        auto other_itr = std::make_shared<LazyIterator<T>>( other );
        return map_filter<U>( [zipper, other_itr]( const T &e ) -> Opt<U> {
            auto next = other_itr->curr();
            if ( !next.has_value() )
                return {};
            other_itr->skip_self( 1 );
            return zipper( e, *next );
        } );
    }

    /// Zips this iterator's elements together with two other iterator's
    /// elements.
    template <typename U>
    LazyIterator<U> zip_with3(
        std::function<U( const T &, const T &, const T & )> zipper,
        LazyIterator<T> other2, LazyIterator<T> other3 ) const {
        auto other_itr2 = std::make_shared<LazyIterator<T>>( other2 );
        auto other_itr3 = std::make_shared<LazyIterator<T>>( other3 );
        return map_filter<U>(
            [zipper, other_itr2, other_itr3]( const T &e ) -> Opt<U> {
                auto next2 = other_itr2->curr();
                if ( !next2.has_value() )
                    return {};
                auto next3 = other_itr3->curr();
                if ( !next3.has_value() )
                    return {};
                other_itr2->skip_self( 1 );
                other_itr3->skip_self( 1 );
                return zipper( e, *next2, *next3 );
            } );
    }

    /// Returns whether all elements fulfill \param predicate.
    bool all( std::function<bool( const T & )> predicate ) const {
        // Manual optimization.
        auto itr = *this;
        while ( itr ) {
            if ( !predicate( itr.consume() ) )
                return false;
        }
        return true;
    }

    /// Returns whether any element fulfills \param predicate.
    bool any( std::function<bool( const T & )> predicate ) const {
        // Manual optimization.
        auto itr = *this;
        while ( itr ) {
            if ( predicate( itr.consume() ) )
                return true;
        }
        return false;
    }

    /// Returns the first element that matches \param predicate. Consider using
    /// filter() instead.
    Opt<T> find( std::function<bool( const T & )> predicate ) const {
        return filter( predicate ).curr();
    }

    /// Returns an iterator with reverse order of this iterator's elements.
    /// There is no limit to the current iterator position, i. e. the returned
    /// iterator can also access previous elements, unless explicitly limited.
    LazyIterator<T> reverse( bool from_end = true ) const {
        auto itr =
            std::make_shared<LazyIterator<T>>( from_end ? end() - 1 : *this );
        return LazyIterator<T>( [itr]() -> Opt<T> {
            if ( itr->idx == 0 )
                return {};
            auto &ret = itr->curr();
            if ( ret.has_value() )
                itr->skip_self( -1 );
            return ret;
        } );
    }

    /// Match against the current element of this iterator. U must overload
    /// operator bool.
    template <typename U>
    U match( U &first ) const {
        return first.match( *this );
    }
    /// Match against elements of this iterator.
    template <typename U, typename... Pack>
    bool match( U &first, Pack &...pack ) const {
        return first.match( *this ) && match( pack... );
    }

    LazyIterator<T> &operator++() { return skip_self( 1 ); }
    LazyIterator<T> &operator--() { return skip_self( -1 ); }
    LazyIterator<T> operator+( ssize_t offset ) const { return skip( offset ); }
    LazyIterator<T> operator-( ssize_t offset ) const {
        return skip( -offset );
    }
    LazyIterator<T> &operator+=( ssize_t offset ) {
        return skip_self( offset );
    }
    LazyIterator<T> &operator-=( ssize_t offset ) {
        return skip_self( -offset );
    }
    explicit operator bool() const { return curr_valid(); }
    bool operator==( const LazyIterator<T> other ) const {
        return state == other.state && idx == other.idx &&
               limit == other.limit;
    }
    bool operator!=( const LazyIterator<T> other ) const {
        return !( *this == other );
    }
};

} // namespace chc
