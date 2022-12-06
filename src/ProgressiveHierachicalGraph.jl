mutable struct LockData{T}
    vertex::HVertex{T}
    paths::BitSet
end

export ProgressiveHierachicalGraph
mutable struct ProgressiveHierachicalGraph{T} <: AbstractGraph{T}
    graph::PathAwareHierachicalGraph{T}
    locks::Vector{LockData{T}}
    keys::Vector{BitSet}
end

# Constructors
function ProgressiveHierachicalGraph(g::PathAwareHierachicalGraph{T}) where T
    locks = LockData{T}[]
    keys = BitSet[]
    return ProgressiveHierachicalGraph{T}(g, locks, keys)
end
@inline ProgressiveHierachicalGraph{T}(entries::Int, exits::Int) where T = 
    ProgressiveHierachicalGraph(PathAwareHierachicalGraph{T}(entries, exits))

# AbstractGraph Interface
@inline Base.eltype(::Type{<:ProgressiveHierachicalGraph}) = Int
@inline Base.eltype(::ProgressiveHierachicalGraph) = Int

@inline Base.zero(::Type{T}) where T <: ProgressiveHierachicalGraph = T(0, 0)

@inline Graphs.is_directed(::Type{<:ProgressiveHierachicalGraph}) = true

@inline Graphs.edges(g::ProgressiveHierachicalGraph) = edges(g.graph)

@inline Graphs.edgetype(::Type{<:ProgressiveHierachicalGraph}) = Edge{Int}
@inline Graphs.edgetype(::ProgressiveHierachicalGraph) = Edge{Int}

@inline Graphs.ne(g::ProgressiveHierachicalGraph) = ne(g.graph)

@inline @propagate_inbounds Graphs.has_edge(g::ProgressiveHierachicalGraph, s, d) = has_edge(g.graph, s, d)

@inline Graphs.vertices(g::ProgressiveHierachicalGraph) = vertices(g.graph)

@inline Graphs.nv(g::ProgressiveHierachicalGraph) = nv(g.graph)

@inline Graphs.has_vertex(g::ProgressiveHierachicalGraph, v) = has_vertex(g.graph, v)

@inline @propagate_inbounds Graphs.inneighbors(g::ProgressiveHierachicalGraph, v) = inneighbors(g.graph, v)

@inline @propagate_inbounds Graphs.outneighbors(g::ProgressiveHierachicalGraph, v) = outneighbors(g.graph, v)

# Hierachical Graph Interface
@inline entries(g::ProgressiveHierachicalGraph) = entries(g.graph)
@inline exits(g::ProgressiveHierachicalGraph) = exits(g.graph)
@inline isentry(g::ProgressiveHierachicalGraph, v) = isentry(g.graph, v)
@inline isexit(g::ProgressiveHierachicalGraph, v) = isexit(g.graph, v)
@inline entry_index(g::ProgressiveHierachicalGraph, v) = entry_index(g.graph, v)
@inline exit_index(g::ProgressiveHierachicalGraph, v) = exit_index(g.graph, v)

function add_subgraph!(f, l, g::ProgressiveHierachicalGraph{T}, v::Int) where T
    add_subgraph!(f, g.graph, v)
    
    subgraph = length(g.graph.graph.subgraphs)
    paths = g.graph.paths[subgraph].paths.data
    local_filter(ps::Vararg{Pair{T, T}, N}; exclusive::Bool = false) where N = filter(g, subgraph, ps...; exclusive)
    local_lock!(local_v::T, ps::Vararg{Pair{T, T}, N}) where N = lock!(g, subgraph, local_v, ps...)

    l(local_filter, local_lock!, paths)

    return g
end
@propagate_inbounds @inline function add_subgraph!(f, l, g::ProgressiveHierachicalGraph, s, d)
    h = g.graph.graph
    v_s = h.vertices[s]
    v_d = h.vertices[d]
    s_outs = outneighbors(@_local h v_s)
    local_d = first(v -> redirect(h, v_s.subgraph, v_s.vertex, v) == v_d, s_outs)
    @assert !isnothing(local_d) "invalid edge $s => $d"
    add_subgraph!(f, l, g, d)

    v_d = redirect(h, v_s.subgraph, v_s.vertex, local_d)
    return index(h.vertices, v_d)
end

# Extensions
export lock!
@propagate_inbounds @inline Base.filter(g::ProgressiveHierachicalGraph{T}, subgraph::Int, paths::Vararg{Pair{T, T}, N}; exclusive::Bool = false) where {N, T} =
    filter(g.graph, subgraph, paths...; exclusive)

function lock!(g::ProgressiveHierachicalGraph{T}, sub::Int, v::T, paths::Vararg{Pair{T, T}, N}) where {T, N}
    @assert HVertex(v, sub) in g.graph.graph.vertices "unknown vertex"
    subgraph = g.graph.graph.subgraphs[sub]
    data = g.graph.paths[sub]
    lock = LockData(HVertex(v, sub), BitSet(index.((data.paths,), paths)))
    push!(g.locks, lock)
    keys = BitSet()
    openlist = Queue{T}()
    enqueue!(openlist, v)
    while !isempty(openlist)
        w = dequeue!(openlist)
        w in keys && continue
        push!(keys, w)
        local_data = data.vertex_data[w]
        for (i, x) in enumerate(inneighbors(subgraph, w))
            # ASSERT: v in filter(..., paths)
            any(p -> let ins = local_data.ins_after[p]
                !isnothing(ins) && i in ins
            end, lock.paths) && continue
            enqueue!(openlist, x)
        end
    end
    push!(g.keys, keys)
    return length(g.locks)
end

export locks
@inline locks(g::ProgressiveHierachicalGraph) = index.((g.graph.graph.vertices,), getfield.(g.locks, :vertex))
@inline Base.keys(g::ProgressiveHierachicalGraph, v::Int) = findall(l -> haskey(g, v, l), 1:length(g.locks))

@propagate_inbounds function Base.haskey(g::ProgressiveHierachicalGraph, v::Int, l::Int)
    !(1 <= l <= length(g.locks)) && return false
    vertex = g.graph.graph.vertices[v]
    lock = g.locks[l]
    if vertex.subgraph == lock.vertex.subgraph
        return vertex.vertex in g.keys[l]
    else
        path = tree_path(g.graph.graph.hierachy, lock.vertex.subgraph, vertex.subgraph)     
        # ASSERT: keys can only be on the same subgraph as the lock (or its sub-graphs)
        isnothing(path) && return false
        return g.graph.graph.index[lock.vertex.subgraph][!, path[2]] in g.keys[l]
    end
end

# TODO: is it more efficient to traverse the hierachy to find all vertices instead?
@propagate_inbounds Base.filter(g::ProgressiveHierachicalGraph, l::Int) = 
    filter(v -> haskey(g, v, l), vertices(g))
