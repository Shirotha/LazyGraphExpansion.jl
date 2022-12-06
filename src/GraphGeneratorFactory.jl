export GraphGeneratorFactory
abstract type GraphGeneratorFactory end

# Interface
subgraph_generator(f::T, s) where T <: GraphGeneratorFactory = 
    error("missing implementation for subgraph_generator(::$T, state)!")
lock_generator(f::T, s) where T <: GraphGeneratorFactory =
    error("missing implementation for lock_generator(::$T, state)!")
should_expand(f::T, g, v, s) where T <: GraphGeneratorFactory =
    error("missing implementation for should_expand(::$T, graph, vertex, state)!")

# Extensions
export move
function move(g, s, d, f::GraphGeneratorFactory, state)
    @assert has_edge(g, s, d) "no valid edge!"
    isexit(g, d) && return d
    
    sub = subgraph_generator(f, state)
    lock = lock_generator(f, state)
    while should_expand(f, g, d, state)
        d = add_subgraph!(sub, lock, g, s, d)
    end
    return d
end
function move(g, s, d)
    @assert has_edge(g, s, d) "no valid edge!"
    return d
end