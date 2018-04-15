#
#
#
lc_sum_number = function(...)
{
    return (sum(...))
}

#
#
#
lc_sum_vector = function(...)
{
    return (sum(...))
}

#
#
#
lc_sum = function(first, second = NULL)
{
    if(is.null(second))
    {
        # Only one data frame
        if(typeof(first) == "list")
        {
            first = data.frame(first)
            return (rowSums(first))   
        }
        # Vector or plain number
        else
        {
            return (sum(first))
        }
    }
    else
    {
        # Two data frames
        if(typeof(first) == typeof(second) & typeof(first) == "list")
        {
            return (first)
        }
        # Two Vectors or plain numbers
        else
        {
            return (sum(first, second))
        }
    }
}

#
#
#
lc_diff = function(first, second)
{
    
}

#
#
#
lc_frac = function(first, second)
{
    
}