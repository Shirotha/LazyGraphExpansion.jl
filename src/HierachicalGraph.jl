using Graphs, DataStructures
using Base:@propagate_inbounds



# Vertex
struct HVertex{T}
    vertex::T
    subgraph::Int
end

# Conversion
@inline Base.convert(::Type{HVertex{T}}, v::HVertex) where T = HVertex{T}(v.vertex, v.subgraph)

# Promotion
@inline Base.promote_rule(::Type{HVertex{T}}, ::Type{HVertex{S}}) where {T, S} = HVertex{promote_type(T, S)}

# Comparision
@inline Base.isequal(a::HVertex, b::HVertex) = a.vertex == b.vertex && a.subgraph == b.subgraph
@inline Base.hash(v::HVertex, h=0) = hash(v.vertex, hash(v.subgraph, UInt64(h)))



# Graph
export HierachicalGraph
mutable struct HierachicalGraph{T <: Integer} <: AbstractGraph{Int}
    # only holds leaf vertices (when sub-graph is added to a vertex replace entry with central vertex in sub-graph)
    vertices::IndexedVector{HVertex{T}}
    hierachy::SimpleDiGraph{T}
    subgraphs::Vector{SimpleDiGraph{T}}
    index::Vector{BiDict{T, Int}}
    # when travering local edge (source => destination): entries[index[destination]][source] == target interface vertex local to subgraphs[index[destination]]
    entries::Vector{BiDict{T, T}}
    # when entering interface vertex: exits[subgraph][interface vertex] == redirect target in parent graph
    exits::Vector{BiDict{T, T}}
end

# Helpers
macro _index(g, v)
    :(($(esc(g)).index[$(esc(v)).subgraph], $(esc(v)).vertex)...)
end
macro _local(g, v)
    :(($(esc(g)).subgraphs[$(esc(v)).subgraph], $(esc(v)).vertex)...)
end

# Contructors
function HierachicalGraph{T}(entries::Int, exits::Int) where T
    n = entries + exits + 1
    g = SimpleDiGraph{T}(n)

    _entries = BiDict{T, T}()
    for i in 1:entries
        add_edge!(g, i, n)
        _entries[i] = i
    end

    _exits = BiDict{T, T}()
    for i in 1:exits
        add_edge!(g, n, entries + i)
        _exits[entries + i] = i
    end

    vertices = IndexedVector{HVertex{T}}([HVertex{T}(i, 1) for i in 1:(entries + exits + 1)])
    hierachy = SimpleDiGraph{T}(1)
    index = [BiDict{T, Int}()]
    return HierachicalGraph(vertices, hierachy, [g], index, [_entries], [_exits])
end
function HierachicalGraph{T}(g::HierachicalGraph) where T
    vertices = convert(IndexedVector{HVertex{T}}, g.vertices)
    hierachy = convert(SimpleDiGraph{T}, g.hierachy)
    subgraphs = convert.(SimpleDiGraph{T}, g.subgraphs)
    index = convert.(BiDict{T, Int}, g.index)
    entries = convert.(BiDict{T, T}, g.entries)
    exits = convert.(BiDict{T, T}, g.exits)
    return HierachicalGraph{T}(vertices, hierachy, subgraphs, index, entries, exits)
end

# Conversion
@inline Base.convert(::Type{T}, g::T) where T <: HierachicalGraph = g
@inline Base.convert(::Type{T}, g::AbstractGraph) where T <: HierachicalGraph = T(g)

# Promotion
Base.promote_rule(::Type{HierachicalGraph{T}}, ::Type{HierachicalGraph{S}}) where {T, S} = HierachicalGraph{promote_type(T, S)}

# Iteration
#@inline Base.length(g::HierachicalGraph) = length(g.subgraphs)
#@inline Base.iterate(g::HierachicalGraph) = isempty(g.subgraphs) ? nothing : (@inbounds g.subgraphs[1], 2)
#@inline Base.iterate(g::HierachicalGraph, i) = 1 <= i <= length(g.subgraphs) ? (@inbounds g.subgraphs[i], i + 1) : nothing

# Redirection
@inline Base.checkbounds(g::HierachicalGraph, subgraph::Int) =
    1 <= subgraph <= length(g.subgraphs) || throw(BoundsError(g, subgraph))
# FIXME: sometimes can cause out-of-bounds access (maybe bad graph?) (missing item in entries)
@propagate_inbounds function redirect(g::HierachicalGraph{T}, subgraph::Int, source::T, destination::T) where T
    @boundscheck checkbounds(g, subgraph)
    @assert has_edge(g.subgraphs[subgraph], source, destination) "invalid redirection $source => $destination (edge does not exist)"

    # ASSERT: interface vertices are leaf vertices
    @inbounds while true
        if haskey(g.exits[subgraph], destination)
            # destination is interface vertex
            # top-level exit
            subgraph == 1 && return HVertex(destination, 1)

            destination = g.exits[subgraph][destination]
            parent = inneighbors(g.hierachy, subgraph)[1]
            source = g.index[parent][!, subgraph]
            subgraph = parent
        elseif haskey(g.index[subgraph], destination)
            # destination has subgraph
            subgraph = g.index[subgraph][destination]
            source = g.entries[subgraph][source]
            # ASSERT: interface vertices have exactly one exit
            destination = outneighbors(g.subgraphs[subgraph], source)[1]
        else
            # destination is leaf
            return HVertex(destination, subgraph)
        end
    end
end
@propagate_inbounds function reverse_redirect(g::HierachicalGraph, subgraph::Int, source::T, destination::T) where T
    @boundscheck checkbounds(g, subgraph)
    @assert has_edge(g.subgraphs[subgraph], source, destination) "invalid redirection $source => $destination (edge does not exist)"

    @inbounds while true
        if hasvalue(g.entries[subgraph], source)
            # source is interface vertex
            # top-level entrance
            subgraph == 1 && return HVertex(source, 1)

            source = g.entries[subgraph][!, source]
            parent = inneighbors(g.hierachy, subgraph)[1]
            destination = g.index[parent][!, subgraph]
            subgraph = parent
        elseif haskey(g.index[subgraph], source)
            # source has subgraph
            subgraph = g.index[subgraph][source]
            destination = g.exits[subgraph][!, source]
            # ASSERT: interface vertices have exactly one entry
            source = inneighbors(g.subgraphs[subgraph], destination)[1]
        else
            # source is leaf
            return HVertex(source, subgraph)
        end
    end
end

# AbstractGraph interface
@inline Base.eltype(::Type{<:HierachicalGraph}) = Int
@inline Base.eltype(::HierachicalGraph) = Int

@inline Base.zero(::Type{T}) where T <: HierachicalGraph = T(0, 0)

@inline Graphs.is_directed(::Type{<:HierachicalGraph}) = true

@inline Graphs.edges(g::HierachicalGraph) = HEdgeIter(g)

@inline Graphs.edgetype(::Type{<:HierachicalGraph}) = Edge{Int}
@inline Graphs.edgetype(::HierachicalGraph) = Edge{Int}

@inline Graphs.ne(g::HierachicalGraph) = sum(g.vertices) do v
    length(outneighbors(@_local g v))
end

@propagate_inbounds function Graphs.has_edge(g::HierachicalGraph, s, d)
    v_s = g.vertices[s]
    v_d = g.vertices[d]
    if v_s.subgraph == v_d.subgraph
        # ASSERT: vertices only holds leaf vertices
        return has_edge((@_local g v_s), v_d.vertex)
    else
        for o in outneighbors(@_local g v_s)
            redirect(g, v_s.subgraph, v_s.vertex, o) == v_d && return true
        end
        return false
    end
end

@inline Graphs.vertices(g::HierachicalGraph) = Base.OneTo(length(g.vertices))

@inline Graphs.nv(g::HierachicalGraph) = length(g.vertices)

@inbounds Graphs.has_vertex(g::HierachicalGraph, v) = 1 <= v <= length(g.vertices)

@propagate_inbounds function Graphs.inneighbors(g::HierachicalGraph, v)
    vertex = g.vertices[v]
    ins = reverse_redirect.((g,), vertex.subgraph, inneighbors(@_local g vertex), vertex.vertex)
    return index.((g.vertices,), ins)
end

@propagate_inbounds function Graphs.outneighbors(g::HierachicalGraph, v)
    vertex = g.vertices[v]
    outs = redirect.((g,), vertex.subgraph, vertex.vertex, outneighbors(@_local g vertex))
    return index.((g.vertices,), outs)
end

# Hierachy
export add_subgraph!
function validate_subgraph(g::HierachicalGraph, i::Int)
    subgraph = g.subgraphs[i]
    entries = g.entries[i]
    exits = g.exits[i]
    @assert nv(subgraph) >= length(union(values(entries), keys(exits))) + 1 "not allowed to have empty sub-graph!"
    @assert begin
        for v in vertices(subgraph)
            ins = length(inneighbors(subgraph, v))
            outs = length(outneighbors(subgraph, v))
            isentry = hasvalue(entries, v)
            isexit = haskey(exits, v)
            @assert !isentry || (outs == 1 && ins <= 1) "entry vertex ($v) can't be modified! (outs = $outs, ins = $ins)"
            @assert !isexit || (ins == 1 && outs <= 1) "exit vertex ($v) can't be modified! (ins = $ins, outs = $outs)"
            @assert isentry || isexit || (ins >= 1 && outs >= 1) "vertex ($v) is deadend or unreachable! (ins = $ins, outs = $outs)"
        end
        true
    end
end

@propagate_inbounds function add_subgraph!(f, g::HierachicalGraph{T}, v) where T
    vertex = g.vertices[v]
    @assert !(vertex.subgraph == 1 && vertex.vertex <= length(g.entries[1]) + length(g.exits[1])) "can't add a subgraph to an entry or exit! ($v)"
    @assert !haskey(@_index g vertex) "vertex ($v) already has a sub-graph!"
    add_vertex!(g.hierachy)
    add_edge!(g.hierachy, vertex.subgraph, nv(g.hierachy))

    subgraph = SimpleDiGraph{T}()
    push!(g.subgraphs, subgraph)
    push!(g.index, BiDict{T, Int}())
    g.index[vertex.subgraph][vertex.vertex] = length(g.subgraphs)
    
    add_vertex!(subgraph)
    g.vertices[v] = HVertex{T}(1, length(g.subgraphs))
    
    entries = BiDict{T, T}()
    push!(g.entries, entries)
    exits = BiDict{T, T}()
    push!(g.exits, exits)
    
    ins = inneighbors(@_local g vertex)
    outs = outneighbors(@_local g vertex)
    interface = union(ins, outs)
    isin = [v in ins for v in interface]
    isout = [v in outs for v in interface]
    
    for (i, v) in enumerate(interface)
        add_vertex!(subgraph)
        if isin[i] 
            add_edge!(subgraph, i + 1, 1)
            entries[v] = i + 1
        end
        if isout[i] 
            add_edge!(subgraph, 1, i + 1)
            exits[i + 1] = v
        end
    end
    
    f(subgraph)

    validate_subgraph(g, length(g.subgraphs))    

    for i in (length(interface) + 2):nv(subgraph)
        push!(g.vertices, HVertex{T}(i, @inbounds g.vertices[v].subgraph))
    end

    return g
end
@propagate_inbounds @inline function add_subgraph!(f, g::HierachicalGraph, s, d)
    add_subgraph!(f, g, d)
    v = g.entries[end][s]
    return v, outneighbors(g.subgraphs[end], v)[1]
end

# Entries/Exits
export entries, exits, isentry, isexit, entry_index, exit_index

# ASSERT: entries have global vertex numbers from 1:entries
@inline entries(g::HierachicalGraph) = @inbounds 1:length(g.entries[1])
# ASSERT: exits have global vertex numbers from (entries + 1):(entries + exits)
@inline exits(g::HierachicalGraph) = @inbounds (1:length(g.exits[1])) .+ length(g.entries[1])
@inline isentry(g::HierachicalGraph, v) = @inbounds 1 <= v <= length(g.entries[1])
@inline isexit(g::HierachicalGraph, v) = @inbounds 1 <= v - length(g.entries[1]) <= length(g.exits[1]) 
@inline entry_index(g::HierachicalGraph, v) = isentry(g, v) ? v : nothing
@inline exit_index(g::HierachicalGraph, v) = isexit(g, v) ? v - length(@inbounds g.entries[1]) : nothing


# Edge Iterator
struct HEdgeIter{T} <: AbstractEdgeIter
    graph::HierachicalGraph{T}
end

# Iteration
@inline Base.iterate(iter::HEdgeIter) = iterate(iter, (1, 1))
function Base.iterate(iter::HEdgeIter, s::Tuple{Int, Int})
    1 <= s[1] <= length(iter.graph.vertices) || return nothing
    # ASSERT: top-level exits have no outgoing edges
    isexit(iter.graph, s[1]) && return iterate(iter, (length(entries(iter.graph)) + length(exits(iter.graph)) + 1, 1))
    v = @inbounds iter.graph.vertices[s[1]]
    outs = outneighbors(@_local iter.graph v)
    1 <= s[2] <= length(outs) || return nothing
    o = redirect(iter.graph, v.subgraph, v.vertex, @inbounds outs[s[2]])
    e = Edge(index(iter.graph.vertices, v), index(iter.graph.vertices, o))
    if s[2] == length(outs)
        # ASSERT: all global vertices have at least one exit
        return e, (s[1] + 1, 1)
    else
        return e, (s[1], s[2] + 1)
    end
end
Base.length(iter::HEdgeIter) = sum(iter.graph.vertices) do v
    length(outneighbors(@_local iter.graph v))
end