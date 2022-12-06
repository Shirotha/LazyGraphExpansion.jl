export PathAwareHierachicalGraph
mutable struct PathAwareHierachicalGraph{T} <: AbstractGraph{T}
    graph::HierachicalGraph{T}
    paths::Vector{PathData{T}}
end

# Constructors
function PathAwareHierachicalGraph(g::HierachicalGraph{T}) where T
    paths = Vector{PathData{T}}(undef, length(g.subgraphs))
    for (i, s) in enumerate(g.subgraphs)
        paths[i] = PathData(s, keys(g.entries[i]), keys(g.exits[i]))
    end
    return PathAwareHierachicalGraph{T}(g, paths)
end
@inline PathAwareHierachicalGraph{T}(entries::Int, exits::Int) where T = 
    PathAwareHierachicalGraph(HierachicalGraph{T}(entries, exits))

# AbstractGraph Interface
@inline Base.eltype(::Type{<:PathAwareHierachicalGraph}) = Int
@inline Base.eltype(::PathAwareHierachicalGraph) = Int

@inline Base.zero(::Type{T}) where T <: PathAwareHierachicalGraph = T(0, 0)

@inline Graphs.is_directed(::Type{<:PathAwareHierachicalGraph}) = true

@inline Graphs.edges(g::PathAwareHierachicalGraph) = edges(g.graph)

@inline Graphs.edgetype(::Type{<:PathAwareHierachicalGraph}) = Edge{Int}
@inline Graphs.edgetype(::PathAwareHierachicalGraph) = Edge{Int}

@inline Graphs.ne(g::PathAwareHierachicalGraph) = ne(g.graph)

@inline @propagate_inbounds Graphs.has_edge(g::PathAwareHierachicalGraph, s, d) = has_edge(g.graph, s, d)

@inline Graphs.vertices(g::PathAwareHierachicalGraph) = vertices(g.graph)

@inline Graphs.nv(g::PathAwareHierachicalGraph) = nv(g.graph)

@inline Graphs.has_vertex(g::PathAwareHierachicalGraph, v) = has_vertex(g.graph, v)

@inline @propagate_inbounds Graphs.inneighbors(g::PathAwareHierachicalGraph, v) = inneighbors(g.graph, v)

@inline @propagate_inbounds Graphs.outneighbors(g::PathAwareHierachicalGraph, v) = outneighbors(g.graph, v)

# Hierachical Graph Interface
@inline entries(g::PathAwareHierachicalGraph) = entries(g.graph)
@inline exits(g::PathAwareHierachicalGraph) = exits(g.graph)
@inline isentry(g::PathAwareHierachicalGraph, v) = isentry(g.graph, v)
@inline isexit(g::PathAwareHierachicalGraph, v) = isexit(g.graph, v)
@inline entry_index(g::PathAwareHierachicalGraph, v) = entry_index(g.graph, v)
@inline exit_index(g::PathAwareHierachicalGraph, v) = exit_index(g.graph, v)

# Helper Class
struct InterfaceData
    ins_after::Maybe{BitSet}
    outs_after::Maybe{BitSet}
end
struct LocalPathData{T}
    paths::IndexedVector{Pair{T, T}}
    interface::Vector{InterfaceData}
end

@inline paths(d::LocalPathData) = d.paths.data

@inline entries(d::LocalPathData) = findall(i -> !isnothing(i.ins_after), d.interface) .+ 1
@inline exits(d::LocalPathData) = findall(i -> !isnothing(i.outs_after), d.interface) .+ 1
@inline isentry(d::LocalPathData, v) = v > 1 && !isnothing(d.interface[v - 1].ins_after)
@inline isexit(d::LocalPathData, v) = v > 1 && !isnothing(d.interface[v - 1].outs_after)

@inline entries(d::LocalPathData, p) = let j = index(d.paths, p)
    findall(i -> !isnothing(i.ins_after) && !(j in i.ins_after), d.interface) .+ 1
end
@inline exits(d::LocalPathData, p) = let j = index(d.paths, p)
    findall(i -> !isnothing(i.outs_after) && (j in i.outs_after), d.interface) .+ 1
end

@propagate_inbounds function add_subgraph!(f, g::PathAwareHierachicalGraph{T}, v) where T
    vertex = g.graph.vertices[v]

    ins = inneighbors(@_local g.graph vertex)
    outs = outneighbors(@_local g.graph vertex)
    interface = union(ins, outs)
    isin = [v in ins for v in interface]
    isout = [v in outs for v in interface]

    path = g.paths[vertex.subgraph]
    add_subgraph!(g.graph, v) do subgraph
        paths = IndexedVector{Pair{T, T}}(path[vertex.vertex])
        interface_data = Vector{InterfaceData}(undef, length(interface))
        for (i, v) in enumerate(interface)
            if isin[i]
                j = findfirst(==(v), ins)
                ins_after = BitSet()
                for (k, p) in enumerate(paths)
                    j in path[vertex.vertex, p, :in] && push!(ins_after, k)
                end
            else
                ins_after = nothing
            end
            if isout[i]
                j = findfirst(==(v), outs)
                outs_after = BitSet()
                for (k, p) in enumerate(paths)
                    j in path[vertex.vertex, p, :out] && push!(outs_after, k)
                end
            else
                outs_after = nothing
            end
            
            @inbounds interface_data[i] = InterfaceData(ins_after, outs_after)
        end
        f(subgraph, LocalPathData{T}(paths, interface_data))
    end

    push!(g.paths, PathData(g.graph.subgraphs[end], values(g.graph.entries[end]), keys(g.graph.exits[end])))
    return g
end
@propagate_inbounds @inline function add_subgraph!(f, g::PathAwareHierachicalGraph, s, d)
    add_subgraph!(f, g, d)
    v = g.graph.entries[end][s]
    return v, outneighbors(g.graph.subgraphs[end], v)[1]
end

# Extensions
@propagate_inbounds function Base.filter(g::PathAwareHierachicalGraph{T}, subgraph::Int, paths::Vararg{Pair{T, T}, N}; exclusive::Bool=false) where {T, N}
    @boundscheck checkbounds(g.graph, subgraph)
    vs = filter(g.paths[subgraph], paths...; exclusive)
    filter!(vs) do v
        !(hasvalue(g.graph.entries[subgraph], v) || haskey(g.graph.exits[subgraph], v))
    end
    return vs
end