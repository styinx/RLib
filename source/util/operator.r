#
#
#
lc_eq = function(first, second)
{
    return ((first == second))
}

#
#
#
lc_neq = function(first, second)
{
    return ((!(lc_eq(first, second))))
}

#
#
#
lc_l = function(first, second)
{
    return ((first < second))
}

#
#
#
lc_g = function(first, second)
{
    return ((first > second))
}

#
#
#
lc_leq = function(first, second)
{
    return ((lc_l(first, second) | lc_eq(first, second)))
}

#
#
#
lc_geq = function(first, second)
{
    return ((lc_g(first, second) | lc_eq(first, second)))
}