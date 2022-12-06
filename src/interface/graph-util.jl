export mov_edge!
function mov_edge!(g::AbstractGraph, a::Pair, b::Integer)
    rem_edge!(g, a.first, b)
    add_edge!(g, a.second, b)
end
function mov_edge!(g::AbstractGraph, a::Integer, b::Pair)
    rem_edge!(g, a, b.first)
    add_edge!(g, a, b.second)
end
function mov_edge!(g::AbstractGraph, a::Pair, b::Pair)
    rem_edge!(g, a.first, b.first)
    add_edge!(g, a.second, b.second)
end

export bi
function bi(f, g, a, b)
    f(g, a, b)
    f(g, b, a)
end
function bi(f, g, a, b, ab::Bool, ba::Bool)
    ab && f(g, a, b)
    ba && f(g, b, a)
end