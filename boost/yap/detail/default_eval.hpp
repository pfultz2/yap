#ifndef BOOST_YAP_DETAIL_DEFAULT_EVAL_HPP_INCLUDED
#define BOOST_YAP_DETAIL_DEFAULT_EVAL_HPP_INCLUDED

#include <boost/yap/expression_fwd.hpp>
#include <boost/yap/operators.hpp>

#include <boost/hana/transform.hpp>

#include <cassert>


namespace boost { namespace yap {

    namespace detail {

        struct nonexistent_transform {};
        inline nonexistent_transform transform_expression (...) { return {}; }

        template <typename I, typename T>
        decltype(auto) eval_placeholder (I, T && arg)
        {
            static_assert(
                I::value == 1,
                "The current placeholder's index is out of bounds.  Did you perhaps call "
                "evaluate() with too few arguments?"
            );
            return static_cast<T &&>(arg);
        }

        template <typename T, typename ...Ts>
        decltype(auto) eval_placeholder (hana::llong<1>, T && arg, Ts && ... args)
        { return static_cast<T &&>(arg); }

        template <typename I, typename T, typename ...Ts>
        decltype(auto) eval_placeholder (I, T && arg, Ts && ... args)
        { return eval_placeholder(hana::llong<I::value - 1>{}, static_cast<Ts &&>(args)...); }

        template <typename Expr, typename ...T>
        decltype(auto) default_eval_expr (Expr && expr, T && ... args);

        template <expr_kind Kind>
        struct default_eval_expr_impl;

        template <>
        struct default_eval_expr_impl<expr_kind::expr_ref>
        {
            template <typename Expr, typename ...T>
            decltype(auto) operator() (Expr && expr, T && ... args)
            { return default_eval_expr(::boost::yap::deref(static_cast<Expr &&>(expr)), static_cast<T &&>(args)...); }
        };

        template <>
        struct default_eval_expr_impl<expr_kind(-1)>
        {
            template <typename Expr, typename ...T>
            decltype(auto) operator() (Expr && expr, T && ... args)
            { return transform_expression(static_cast<Expr &&>(expr), static_cast<T &&>(args)...); }
        };

        template <>
        struct default_eval_expr_impl<expr_kind::terminal>
        {
            template <typename Expr, typename ...T>
            decltype(auto) operator() (Expr && expr, T && ... args)
            { return ::boost::yap::value(static_cast<Expr &&>(expr)); }
        };

        template <>
        struct default_eval_expr_impl<expr_kind::placeholder>
        {
            template <typename Expr, typename ...T>
            decltype(auto) operator() (Expr && expr, T && ... args)
            { return eval_placeholder(::boost::yap::value(static_cast<Expr &&>(expr)), static_cast<T &&>(args)...); }
        };

#define BOOST_YAP_UNARY_OPERATOR_CASE(op_name)                          \
        template <>                                                     \
        struct default_eval_expr_impl<expr_kind:: op_name>              \
        {                                                               \
            template <typename Expr, typename ...T>                     \
            decltype(auto) operator() (Expr && expr, T && ... args)     \
            {                                                           \
                using namespace hana::literals;                         \
                return eval_ ## op_name(                                \
                    default_eval_expr(static_cast<Expr &&>(expr).elements[0_c], static_cast<T &&>(args)...) \
                );                                                      \
            }                                                           \
        };

        BOOST_YAP_UNARY_OPERATOR_CASE(unary_plus) // +
        BOOST_YAP_UNARY_OPERATOR_CASE(negate) // -
        BOOST_YAP_UNARY_OPERATOR_CASE(dereference) // *
        BOOST_YAP_UNARY_OPERATOR_CASE(complement) // ~
        BOOST_YAP_UNARY_OPERATOR_CASE(address_of) // &
        BOOST_YAP_UNARY_OPERATOR_CASE(logical_not) // !
        BOOST_YAP_UNARY_OPERATOR_CASE(pre_inc) // ++
        BOOST_YAP_UNARY_OPERATOR_CASE(pre_dec) // --
        BOOST_YAP_UNARY_OPERATOR_CASE(post_inc) // ++(int)
        BOOST_YAP_UNARY_OPERATOR_CASE(post_dec) // --(int)

#undef BOOST_YAP_UNARY_OPERATOR_CASE

#define BOOST_YAP_BINARY_OPERATOR_CASE(op_name)                         \
        template <>                                                     \
        struct default_eval_expr_impl<expr_kind:: op_name>              \
        {                                                               \
            template <typename Expr, typename ...T>                     \
            decltype(auto) operator() (Expr && expr, T && ... args)     \
            {                                                           \
                using namespace hana::literals;                         \
                return eval_ ## op_name(                                \
                    default_eval_expr(static_cast<Expr &&>(expr).elements[0_c], static_cast<T &&>(args)...), \
                    default_eval_expr(static_cast<Expr &&>(expr).elements[1_c], static_cast<T &&>(args)...) \
                );                                                      \
            }                                                           \
        };

        BOOST_YAP_BINARY_OPERATOR_CASE(shift_left) // <<
        BOOST_YAP_BINARY_OPERATOR_CASE(shift_right) // >>
        BOOST_YAP_BINARY_OPERATOR_CASE(multiplies) // *
        BOOST_YAP_BINARY_OPERATOR_CASE(divides) // /
        BOOST_YAP_BINARY_OPERATOR_CASE(modulus) // %
        BOOST_YAP_BINARY_OPERATOR_CASE(plus) // +
        BOOST_YAP_BINARY_OPERATOR_CASE(minus) // -
        BOOST_YAP_BINARY_OPERATOR_CASE(less) // <
        BOOST_YAP_BINARY_OPERATOR_CASE(greater) // >
        BOOST_YAP_BINARY_OPERATOR_CASE(less_equal) // <=
        BOOST_YAP_BINARY_OPERATOR_CASE(greater_equal) // >=
        BOOST_YAP_BINARY_OPERATOR_CASE(equal_to) // ==
        BOOST_YAP_BINARY_OPERATOR_CASE(not_equal_to) // !=
        BOOST_YAP_BINARY_OPERATOR_CASE(logical_or) // ||
        BOOST_YAP_BINARY_OPERATOR_CASE(logical_and) // &&
        BOOST_YAP_BINARY_OPERATOR_CASE(bitwise_and) // &
        BOOST_YAP_BINARY_OPERATOR_CASE(bitwise_or) // |
        BOOST_YAP_BINARY_OPERATOR_CASE(bitwise_xor) // ^

        template <>
        struct default_eval_expr_impl<expr_kind::comma>
        {
            template <typename Expr, typename ...T>
            decltype(auto) operator() (Expr && expr, T && ... args)
            {
                using namespace hana::literals;
                return eval_comma(
                    default_eval_expr(static_cast<Expr &&>(expr).elements[0_c], static_cast<T &&>(args)...),
                    default_eval_expr(static_cast<Expr &&>(expr).elements[1_c], static_cast<T &&>(args)...)
                );
            }
        };

        BOOST_YAP_BINARY_OPERATOR_CASE(mem_ptr) // ->*
        BOOST_YAP_BINARY_OPERATOR_CASE(assign) // =
        BOOST_YAP_BINARY_OPERATOR_CASE(shift_left_assign) // <<=
        BOOST_YAP_BINARY_OPERATOR_CASE(shift_right_assign) // >>=
        BOOST_YAP_BINARY_OPERATOR_CASE(multiplies_assign) // *=
        BOOST_YAP_BINARY_OPERATOR_CASE(divides_assign) // /=
        BOOST_YAP_BINARY_OPERATOR_CASE(modulus_assign) // %=
        BOOST_YAP_BINARY_OPERATOR_CASE(plus_assign) // +=
        BOOST_YAP_BINARY_OPERATOR_CASE(minus_assign) // -=
        BOOST_YAP_BINARY_OPERATOR_CASE(bitwise_and_assign) // &=
        BOOST_YAP_BINARY_OPERATOR_CASE(bitwise_or_assign) // |=
        BOOST_YAP_BINARY_OPERATOR_CASE(bitwise_xor_assign) // ^=
        BOOST_YAP_BINARY_OPERATOR_CASE(subscript) // []

#undef BOOST_YAP_BINARY_OPERATOR_CASE

        template <>
        struct default_eval_expr_impl<expr_kind::if_else>
        {
            template <typename Expr, typename ...T>
            decltype(auto) operator() (Expr && expr, T && ... args)
            {
                using namespace hana::literals;
                return eval_if_else(
                    default_eval_expr(static_cast<Expr &&>(expr).elements[0_c], static_cast<T &&>(args)...),
                    default_eval_expr(static_cast<Expr &&>(expr).elements[1_c], static_cast<T &&>(args)...),
                    default_eval_expr(static_cast<Expr &&>(expr).elements[2_c], static_cast<T &&>(args)...)
                );
            }
        };

        template <>
        struct default_eval_expr_impl<expr_kind::call>
        {
            template <typename Expr, typename ...T>
            decltype(auto) operator() (Expr && expr, T && ... args)
            {
                auto expand_args = [&](auto && element) {
                    return default_eval_expr(
                        static_cast<decltype(element) &&>(element),
                        static_cast<T &&>(args)...
                    );
                };

                return hana::unpack(
                    static_cast<Expr &&>(expr).elements,
                    [expand_args](auto && ... elements) {
                        return eval_call(
                            expand_args(static_cast<decltype(elements) &&>(elements))...
                        );
                    }
                );
            }
        };

        template <typename Expr, typename ...T>
        decltype(auto) default_eval_expr (Expr && expr, T && ... args)
        {
            constexpr bool transform_exists = !std::is_same<
                decltype(transform_expression(static_cast<Expr &&>(expr), static_cast<T &&>(args)...)),
                nonexistent_transform
            >{};
            constexpr expr_kind kind = transform_exists ?
                expr_kind(-1) :
                remove_cv_ref_t<Expr>::kind;
            return default_eval_expr_impl<kind>{}(static_cast<Expr &&>(expr), static_cast<T &&>(args)...);
        }

        template <typename Expr, typename Tuple, typename Transform>
        auto transform_nonterminal (Expr const & expr, Tuple && tuple, Transform && transform);

        template <
            typename Expr,
            typename Transform,
            expr_kind Kind,
            bool IsExprRef = Kind == expr_kind::expr_ref,
            bool IsTerminalOrPlaceholder = Kind == expr_kind::terminal || Kind == expr_kind::placeholder,
            bool IsLvalueRef = std::is_lvalue_reference<Expr>{}
        >
        struct default_transform_expression_impl;

        template <
            typename Expr,
            typename Transform,
            expr_kind Kind,
            bool IsTerminalOrPlaceholder,
            bool IsLvalueRef
        >
        struct default_transform_expression_impl <
            Expr,
            Transform,
            Kind,
            true,
            IsTerminalOrPlaceholder,
            IsLvalueRef
        > {
            auto operator() (Expr && expr, Transform && transform)
            {
                return ::boost::yap::transform(
                    ::boost::yap::deref(expr),
                    static_cast<Transform &&>(transform)
                );
            }
        };

        template <typename Expr, typename Transform, expr_kind Kind, bool IsLvalueRef>
        struct default_transform_expression_impl <
            Expr,
            Transform,
            Kind,
            false,
            true,
            IsLvalueRef
        > {
            auto operator() (Expr && expr, Transform && transform)
            { return static_cast<Expr &&>(expr); }
        };

        template <typename Expr, typename Transform, expr_kind Kind>
        struct default_transform_expression_impl <Expr, Transform, Kind, false, false, true>
        {
            auto operator() (Expr && expr, Transform && transform)
            {
                return transform_nonterminal(
                    expr,
                    expr.elements,
                    static_cast<Transform &&>(transform)
                );
            }
        };

        template <typename Expr, typename Transform, expr_kind Kind>
        struct default_transform_expression_impl <Expr, Transform, Kind, false, false, false>
        {
            auto operator() (Expr && expr, Transform && transform)
            {
                return transform_nonterminal(
                    expr,
                    std::move(expr.elements),
                    static_cast<Transform &&>(transform)
                );
            }
        };

        template <typename Expr, typename Transform, expr_arity Arity, typename = detail::void_t<>>
        struct default_transform_expression
        {
            auto operator() (Expr && expr, Transform && transform)
            {
                constexpr expr_kind kind = remove_cv_ref_t<Expr>::kind;
                return default_transform_expression_impl<Expr, Transform, kind>{}(
                    static_cast<Expr &&>(expr),
                    static_cast<Transform &&>(transform)
                );
            }
        };

        template <typename Expr, typename Transform, expr_arity Arity>
        struct default_transform_expression<
            Expr,
            Transform,
            Arity,
            detail::void_t<decltype(std::declval<Transform>()(std::declval<Expr>()))>
        >
        {
            decltype(auto) operator() (Expr && expr, Transform && transform)
            { return static_cast<Transform &&>(transform)(static_cast<Expr &&>(expr)); }
        };

        template <typename Expr, typename Transform>
        struct default_transform_expression<
            Expr,
            Transform,
            expr_arity::one,
            detail::void_t<decltype(
                std::declval<Transform>()(
                    detail::tag_for<remove_cv_ref_t<Expr>::kind>(),
                    ::boost::yap::value(::boost::yap::value(std::declval<Expr>()))
                )
            )>
        >
        {
            decltype(auto) operator() (Expr && expr, Transform && transform)
            {
                return static_cast<Transform &&>(transform)(
                    detail::tag_for<remove_cv_ref_t<Expr>::kind>(),
                    ::boost::yap::value(::boost::yap::value(static_cast<Expr &&>(expr)))
                );
            }
        };

        template <typename Expr, typename Transform>
        struct default_transform_expression<
            Expr,
            Transform,
            expr_arity::two,
            detail::void_t<decltype(
                std::declval<Transform>()(
                    detail::tag_for<remove_cv_ref_t<Expr>::kind>(),
                    ::boost::yap::value(::boost::yap::left(std::declval<Expr>())),
                    ::boost::yap::value(::boost::yap::right(std::declval<Expr>()))
                )
            )>
        >
        {
            decltype(auto) operator() (Expr && expr, Transform && transform)
            {
                return static_cast<Transform &&>(transform)(
                    detail::tag_for<remove_cv_ref_t<Expr>::kind>(),
                    ::boost::yap::value(::boost::yap::left(static_cast<Expr &&>(expr))),
                    ::boost::yap::value(::boost::yap::right(static_cast<Expr &&>(expr)))
                );
            }
        };

        template <typename Expr, typename Transform>
        struct default_transform_expression<
            Expr,
            Transform,
            expr_arity::three,
            detail::void_t<decltype(
                std::declval<Transform>()(
                    detail::tag_for<remove_cv_ref_t<Expr>::kind>(),
                    ::boost::yap::value(::boost::yap::cond(std::declval<Expr>())),
                    ::boost::yap::value(::boost::yap::then(std::declval<Expr>())),
                    ::boost::yap::value(::boost::yap::else_(std::declval<Expr>()))
                )
            )>
        >
        {
            decltype(auto) operator() (Expr && expr, Transform && transform)
            {
                return static_cast<Transform &&>(transform)(
                    detail::tag_for<remove_cv_ref_t<Expr>::kind>(),
                    ::boost::yap::value(::boost::yap::cond(static_cast<Expr &&>(expr))),
                    ::boost::yap::value(::boost::yap::then(static_cast<Expr &&>(expr))),
                    ::boost::yap::value(::boost::yap::else_(static_cast<Expr &&>(expr)))
                );
            }
        };

        template <typename Expr, typename Transform>
        struct transform_call_unpacker
        {
            template <long long ...I>
            auto operator() (
                Expr && expr,
                Transform && transform,
                std::integer_sequence<long long, I...>
            ) -> decltype(
                static_cast<Transform &&>(transform)(
                    call_tag{},
                    ::boost::yap::value(::boost::yap::argument(
                        static_cast<Expr &&>(expr),
                        hana::llong_c<I>
                    ))...
                )
            ) {
                return static_cast<Transform &&>(transform)(
                    call_tag{},
                    ::boost::yap::value(::boost::yap::argument(
                        static_cast<Expr &&>(expr),
                        hana::llong_c<I>
                    ))...
                );
            }
        };

        template <typename Expr>
        constexpr auto indices_for (Expr const & expr)
        {
            constexpr long long size = decltype(hana::size(expr.elements))::value;
            return std::make_integer_sequence<long long, size>();
        }

        template <typename Expr, typename Transform>
        struct default_transform_expression<
            Expr,
            Transform,
            expr_arity::n,
            detail::void_t<decltype(
                transform_call_unpacker<Expr, Transform>{}(
                    std::declval<Expr>(),
                    std::declval<Transform>(),
                    indices_for(std::declval<Expr>())
                )
            )>
        >
        {
            decltype(auto) operator() (Expr && expr, Transform && transform)
            {
                return transform_call_unpacker<Expr, Transform>{}(
                    static_cast<Expr &&>(expr),
                    static_cast<Transform &&>(transform),
                    indices_for(expr)
                );
            }
        };

        template <
            template<expr_kind, class> class ExprTemplate,
            expr_kind Kind,
            typename OldTuple,
            typename NewTuple
        >
        auto make_expr_from_tuple (ExprTemplate<Kind, OldTuple> const & expr, NewTuple && tuple)
        { return ExprTemplate<Kind, NewTuple>{std::move(tuple)}; }

        template <typename Expr, typename Tuple, typename Transform>
        auto transform_nonterminal (Expr const & expr, Tuple && tuple, Transform && transform)
        {
            auto transformed_tuple = hana::transform(
                static_cast<Tuple &&>(tuple),
                [&transform](auto && element) {
                    using element_t = decltype(element);
                    constexpr expr_kind kind = remove_cv_ref_t<element_t>::kind;
                    default_transform_expression<element_t, Transform, detail::arity_of<kind>()> transformer;
                    return transformer(
                        static_cast<element_t &&>(element),
                        static_cast<Transform &&>(transform)
                    );
                }
            );
            return make_expr_from_tuple(expr, std::move(transformed_tuple));
        }

    }

} }

#endif
