using Graphs, LazyGraphExpansion, SIMD, StaticArrays, Random
using Base:@propagate_inbounds

LGE = LazyGraphExpansion

mutable struct LayoutGraph{T <: Real, I, RNG <: AbstractRNG} <: AbstractGraph{I}
    graph::ProgressiveHierachicalGraph{I}
    positions::Vector{Vec{2, T}}
    pins::BitVector

    edge_length::T
    initial_temperature::T
    iterations::Int

    rng::RNG
end

# Constructors
function LayoutGraph{T, I}(entries::Int, exits::Int; edge_length=1.0, initial_temperature=1.0, iterations=100, rng=Random.GLOBAL_RNG) where {T, I}
    g = ProgressiveHierachicalGraph{I}(entries, exits)
    
    positions = Vector{Vec{2, T}}(undef, entries + exits + 1)
    for v in LGE.entries(g)
        positions[v] = Vec{2, T}((-edge_length, v - 1))
    end
    for v in LGE.exits(g)
        positions[v] = Vec{2, T}((edge_length, v - entries - 1))
    end
    positions[end] = Vec{2, T}((0, 0))

    pins = falses(entries + exits + 1)

    layout = LayoutGraph{T, I, typeof(rng)}(g, positions, pins, edge_length, initial_temperature, iterations, rng)
    layout!(layout)
    return layout
end

# AbstractGraph Interface
@inline Base.eltype(::Type{<:LayoutGraph}) = Int
@inline Base.eltype(::LayoutGraph) = Int

@inline Base.zero(::Type{T}) where T <: LayoutGraph = T(0, 0)

@inline Graphs.is_directed(::Type{<:LayoutGraph}) = true

@inline Graphs.edges(g::LayoutGraph) = edges(g.graph)

@inline Graphs.edgetype(::Type{<:LayoutGraph}) = Edge{Int}
@inline Graphs.edgetype(::LayoutGraph) = Edge{Int}

@inline Graphs.ne(g::LayoutGraph) = ne(g.graph)

@inline @propagate_inbounds Graphs.has_edge(g::LayoutGraph, s, d) = has_edge(g.graph, s, d)

@inline Graphs.vertices(g::LayoutGraph) = vertices(g.graph)

@inline Graphs.nv(g::LayoutGraph) = nv(g.graph)

@inline Graphs.has_vertex(g::LayoutGraph, v) = has_vertex(g.graph, v)

@inline @propagate_inbounds Graphs.inneighbors(g::LayoutGraph, v) = inneighbors(g.graph, v)

@inline @propagate_inbounds Graphs.outneighbors(g::LayoutGraph, v) = outneighbors(g.graph, v)

# Hierachical Graph Interface
@inline LGE.entries(g::LayoutGraph) = entries(g.graph)
@inline LGE.exits(g::LayoutGraph) = exits(g.graph)
@inline LGE.isentry(g::LayoutGraph, v) = isentry(g.graph, v)
@inline LGE.isexit(g::LayoutGraph, v) = isexit(g.graph, v)
@inline LGE.entry_index(g::LayoutGraph, v) = entry_index(g.graph, v)
@inline LGE.exit_index(g::LayoutGraph, v) = exit_index(g.graph, v)

function LGE.add_subgraph!(f, l, g::LayoutGraph{T}, s::Int, d::Int) where T
    d = add_subgraph!(f, l, g.graph, s, d)

    v0 = length(g.positions)
    v1 = nv(g)
    if v1 > v0
        resize!(g.positions, v1)
        P = @inbounds g.positions[d]
        for v in v0:v1
            # TODO: choose better positions based on PathData (find layout in local graph as initial positions?)
            e = Vec{2, T}(sincos(2π * rand(g.rng)))
            r = 0.5 * g.edge_length * rand(g.rng) + 0.1
            @inbounds g.positions[v] = P + r * e
        end

        resize!(g.pins, v1)
        @inbounds view(g.pins, v0:v1) .= false
    end

    layout!(g)
    return d
end

# Extensions
function layout!(g::LayoutGraph{X}) where X
    P = Vec{2, X}
    N = nv(g)
    Nin = length(entries(g))
    Nout = length(exits(g))
    
    K = g.edge_length
    K2 = K^2

    force = similar(g.positions)
    
    𝔉in = P((-K2 / 2, 0))
    𝔉out = P((K2 / 2, 0))
    𝔉0 = P((0, 0))

    # TODO: is it faster to go over the complete range, but have access in order to force instead?
    @inbounds for t in 1:g.iterations
        view(force, 1:Nin) .= (𝔉in,)
        view(force, (Nin + 1):(Nin + Nout)) .= (𝔉out,)
        view(force, (Nin + Nout + 1):N) .= (𝔉0,)

        for i in 1:N
            for j in (i + 1):N
                Δ𝔯 = g.positions[i] - g.positions[j]
                R2 = sum(Δ𝔯^2)
                # FIXME: R2 is NaN sometimes
                @assert R2 > 1e-10 "vertices inside of each other! norm2($i, $j) = $(R2)"
                F = -K2 / R2
                (has_edge(g, i, j) || has_edge(g, j, i)) && (F += sqrt(R2) / K)
                
                force[i] -= F * Δ𝔯
                force[j] += F * Δ𝔯
            end
        end

        T = g.initial_temperature / t
        T2 = T^2

        for i in 1:N
            𝔉 = force[i]
            F2 = sum(𝔉^2)
            if F2 > T2
                g.positions[i] += 𝔉 * T / sqrt(F2)
            else
                g.positions[i] += 𝔉
            end
            @assert !any(isnan(g.positions[i])) "position $i is NaN!"
        end
    end

    return nothing
end

@inline @propagate_inbounds pin!(g::LayoutGraph, v::Int) = g.pins[v] = true
@inline @propagate_inbounds ispinned(g::LayoutGraph, v::Int) = g.pins[v]