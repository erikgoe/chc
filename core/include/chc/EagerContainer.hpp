#pragma once
#include "pch.hpp"

namespace chc {

/// A generic container class for the compiler.
template <typename T>
class EagerContainer {
    std::deque<T> data;

public:
    class Iterator {
        EagerContainer *cont;
        size_t idx = 0;

        Iterator( EagerContainer &container, size_t index )
                : cont( &container ), idx( index ) {}
        friend class EagerContainer;

    public:
        Iterator( const Iterator &other )
                : cont( other.cont ), idx( other.idx ) {}

        bool curr_valid() const { return curr().has_value(); }
        bool curr_not_valid() const { return !curr().has_value(); }
        Opt<std::reference_wrapper<const T>> curr() const {
            if ( idx >= cont->length() )
                return {}; // Upper limit of elements reached.
            return std::cref( cont->data.at( idx ) );
        }
        Opt<std::reference_wrapper<T>> curr() {
            if ( idx >= cont->length() )
                return {}; // Upper limit of elements reached.
            return std::ref( cont->data.at( idx ) );
        }
        /// Unsafe variant of curr()
        const T &get() const {
            assert( curr_valid() );
            return *curr();
        }
        /// Unsafe variant of curr()
        T &get() {
            assert( curr_valid() );
            return *curr();
        }

        /// Safe version of get() which returns an alternative, if the element
        /// would be None.
        const T &get_or( const T &alternative ) const {
            auto ret = curr();
            if ( ret.has_value() ) {
                return *ret;
            } else {
                return alternative;
            }
        }

        /// Safe version of get() which returns an alternative, if the element
        /// would be None.
        T &get_or( T &alternative ) {
            auto ret = curr();
            if ( ret.has_value() ) {
                return *ret;
            } else {
                return alternative;
            }
        }

        /// Does both get the value and increment the iterator.
        const T &consume() {
            assert( curr_valid() );
            auto ret = curr();
            skip_self( 1 );
            return *ret;
        }

        /// Is for consume(), what get_or() is for get().
        const T &consume_or( const T &alternative ) {
            auto ret = curr();
            skip_self( 1 );
            if ( ret.has_value() ) {
                return *ret;
            } else {
                return alternative;
            }
        }

        /// Increment the iterator.
        void consume_opt() { skip_self( 1 ); }

        /// Returns a new iterator with the specified index applied.
        Iterator skip( ssize_t count = 1 ) const {
            Iterator ret( *this );
            ret.skip_self( count );
            return ret;
        }
        /// Jumps given elements forward/backward.
        Iterator &skip_self( ssize_t count = 1 ) {
            if ( count < 0 && -count > static_cast<ssize_t>( idx ) ) {
                idx = 0; // Don't underflow.
            } else {
                idx += count;
            }
            return *this;
        }

        /// Removes the element this iterator points to from the container.
        /// After that it points to the element after the erased one. Other
        /// iterators that point to higher elements will be invalidated.
        void erase_self() { cont->data.erase( cont->data.begin() + idx ); }

        /// Returns a container that contains all elements represented by this
        /// iterator (onwards).
        EagerContainer collect() {
            EagerContainer cont;
            auto itr = *this;
            cont.fill( []( size_t ) {
                auto e = curr();
                skip_self( 1 );
                return e;
            } );
            return cont;
        }

        /// Match against the current element of this iterator. U must overload
        /// operator bool.
        template <typename U>
        bool match( U first ) const {
            return curr_valid() && first.match( get() );
        }
        /// Match against the whole iterator.
        bool match( Iterator pattern ) const {
            return match( pattern.get() ) &&
                   ( pattern.skip( 1 ) ? skip( 1 ).match( pattern.skip( 1 ) )
                                       : true );
        }
        /// Match against elements of this iterator.
        template <typename U, typename... Pack>
        bool match( U first, Pack... pack ) const {
            return match( first ) && skip( 1 ).match( pack... );
        }

        Iterator &operator++() { return skip_self( 1 ); }
        Iterator &operator--() { return skip_self( -1 ); }
        Iterator operator+( ssize_t offset ) const { return skip( offset ); }
        Iterator operator-( ssize_t offset ) const { return skip( -offset ); }
        Iterator &operator+=( ssize_t offset ) { return skip_self( offset ); }
        Iterator &operator-=( ssize_t offset ) { return skip_self( -offset ); }
        explicit operator bool() const { return curr_valid(); }
        bool operator==( const Iterator other ) const {
            return cont == other.cont && idx == other.idx;
        }
        bool operator!=( const Iterator other ) const {
            return !( *this == other );
        }
        bool operator<( const Iterator other ) const {
            return cont == other.cont && idx < other.idx;
        }
    };
    friend class Iterator;

    EagerContainer() {}
    EagerContainer( const EagerContainer<T> &other ) { data = other.data; }

    /// Fills the container with elements that are generated by \param
    /// generator.
    void fill( std::function<Opt<T>( size_t )> generator ) {
        size_t i = 0;
        auto e = generator( i );
        while ( e.has_value() ) {
            data.push_back( e.value() );
            i++;
            e = generator( i );
        }
    }

    /// Appends a new element.
    void put( const T &element ) { data.push_back( element ); }

    /// Inserts an element into the container.
    void insert( Iterator pos, const T &element ) {
        data.insert( data.begin() + pos.idx, element );
    }

    /// Returns an iterator representing the first element.
    Iterator begin() { return Iterator( *this, 0 ); }

    /// Alias for begin()
    Iterator itr() { return begin(); }

    /// Returns an iterator representing the virtual element after the last
    /// element.
    Iterator end() { return Iterator( *this, length() ); }

    /// Returns whether this container has no elements.
    bool empty() const { return data.empty(); }

    /// Returns whether this container has at least one element.
    bool not_empty() const { return !data.empty(); }

    /// Returns the first element.
    Opt<std::reference_wrapper<const T>> first() const {
        if ( empty() )
            return {};
        return data.front();
    }

    /// Returns the last element.
    Opt<std::reference_wrapper<const T>> last() const {
        if ( empty() )
            return {};
        return data.back();
    }

    /// Evaluation of all elements using \param func.
    void for_each( std::function<void( const T & )> func ) const {
        for ( size_t i = 0; i < data.size(); i++ )
            func( data[i] );
    }

    /// Maps all elements to new elements.
    template <typename U>
    EagerContainer<U> map( std::function<U( const T & )> func ) const {
        EagerContainer<U> cont;
        for_each( [&]( const T &e ) { cont.put( func( e ) ); } );
        return cont;
    }

    /// Returns a container with elements, which fulfill \param predicate.
    EagerContainer<T> filter(
        std::function<bool( const T & )> predicate ) const {
        EagerContainer<T> cont;
        for_each( [&]( const T &e ) {
            if ( predicate( e ) )
                cont.put( e );
        } );
        return cont;
    }

    /// Returns a container with elements mapped to new elements and filtered
    /// out if \param func returns None.
    template <typename U>
    EagerContainer<U> map_filter(
        std::function<Opt<U>( const T & )> func ) const {
        EagerContainer<U> cont;
        for_each( [&]( const T &e ) {
            auto n = func( e );
            if ( n )
                cont.put( n.value() );
        } );
        return cont;
    }

    /// Reduces all elements to a single value by accumulating
    /// them using \param func starting with \param init.
    template <typename U>
    U fold( std::function<U( const T &, const U & )> func,
            const U &init ) const {
        auto acc = init;
        for_each( [&]( const T &e ) { acc = func( e, acc ); } );
        return acc;
    }

    /// Reduces all elements to a single value by accumulating
    /// them using \param func starting with \param init. \param func must
    /// combine T back into its parameter.
    template <typename U>
    U fold( std::function<void( const T &, U & )> func, const U &init ) const {
        auto acc = init;
        for_each( [&]( const T &e ) { func( e, acc ); } );
        return acc;
    }

    /// Returns the amount of elements.
    size_t length() const { return data.size(); }

    /// Concatenates all elements together using delimiter between them.
    T concat( const T &delimiter = T() ) const {
        T acc = first().value_or( T{} );
        for ( size_t i = 1; i < data.size(); i++ )
            acc += delimiter + data[i];
        return acc;
    }

    /// Returns whether all elements fulfill \param predicate.
    bool all( std::function<bool( const T & )> predicate ) const {
        // Manual optimization.
        for ( size_t i = 0; i < data.size(); i++ )
            if ( !predicate( data[i] ) )
                return false;
        return true;
    }

    /// Returns whether at least one element fulfills \param predicate.
    bool any( std::function<bool( const T & )> predicate ) const {
        // Manual optimization.
        for ( size_t i = 0; i < data.size(); i++ )
            if ( predicate( data[i] ) )
                return true;
        return false;
    }

    /// Returns the first element that matches \param predicate. Consider using
    /// filter() instead.
    Opt<T> find( std::function<bool( const T & )> predicate ) const {
        for ( size_t i = 0; i < data.size(); i++ )
            if ( predicate( data[i] ) )
                return data[i];
        return {};
    }

    /// Returns a containers with reverse order of this containers's elements.
    EagerContainer<T> reverse() const {
        EagerContainer<T> cont;
        for ( size_t i = 0; i < data.size; i++ )
            cont.put( data.at( data.size() - i - 1 ) );
        return cont;
    }
};

} // namespace chc
