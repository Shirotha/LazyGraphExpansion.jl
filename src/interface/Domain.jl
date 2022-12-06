abstract type Topology{N} <: AbstractVector{Topology{1}} end

# abstract interface
@propagate_inbounds @inline index(s::Topology{N}, axes::NTuple{N, <:AbstractUnitRange}, I::Vararg{Int, N}) where N = 
    index.(s, axes, I)

@interface index(s::Topology{1}, axis::AbstractUnitRange, i::Int)

# AbstractArray interface
@inline Base.size(s::Topology{N}) where N = (N,)
@inline Base.length(s::Topology{N}) where N = N
@inline Base.length(::Type{<:Topology{N}}) where N = N
@inline Base.IndexStyle(::Type{<:Topology}) = IndexLinear()

@interface Base.getindex(s::Topology, i::Int)

# Extensions



abstract type Isotrope{N} <: Topology{N}
end
@inline function Base.getindex(x::Isotrope{N}, i::Int) where {N, T}
    @boundscheck 1 <= i <= N || throw(BoundsError(s, i))
    return first(x)
end

# NOTE: workaround for ghosted range: use Clamped with data of intended size + 2
struct Clamped{N} <: Isotrope{N}
end
@inline Base.first(::Clamped) = Clamped{1}()
@inline index(::Clamped{1}, axis::AbstractUnitRange, i::Int) = clamp(i, axis)

struct Periodic{N} <: Isotrope{N}
end
@inline Base.first(::Periodic) = Periodic{1}()
@inline index(::Periodic{1}, axis::AbstractUnitRange, i::Int) =
    mod(i - first(axis), length(axis)) + first(axis)

struct Mirror{N} <: Isotrope{N}
end
@inline Base.first(::Mirror) = Mirror{1}()
@inline function index(::Mirror{1}, axis::AbstractUnitRange, i::Int)
    i = mod(i - first(axis), 2length(axis) - 1)
    return (i >= length(axis) ? 2length(axis) - i - 2 : i) + first(axis)
end
# FIXME: topologies should be size 0
struct Unisotrope{N, T, I} <: Topology{N}
    topologies::T

    function Unisotrope(args::Vararg{<:Topology, M}) where M
        T = typeof(args)

        Ns = length.(T.parameters)
        N = sum(Ns)

        i = 0
        s = 0
        is = Vector{Int}(undef, N)
        @inbounds for j in 1:N
            if j > s
                i += 1
                s += Ns[i]
            end
            is[j] = i
        end
        I = Tuple{is...}

        return new{N, T, I}(args)
    end
end
@inline function Base.getindex(s::Unisotrope{N, T, I}, i::Int) where {N, T, I}
    @boundscheck 1 <= i <= N || throw(BoundsError(s, i))
    return @inbounds s.topologies[I.parameters[i]]
end
# NOTE: index will never be called and doesn't need an implementation



struct Domain{T, N, X <: Topology{N}, A} <: AbstractArray{T, N}
    data::A
    topology::X

    function Domain(data, topology::X) where X <: Topology
        T = eltype(data)
        N = ndims(data)
        A = typeof(data)
        return new{T, N, X, A}(data, topology)
    end
end

# AbstractArray interface
Base.size(d::Domain) = size(d.data)
@inline Base.IndexStyle(::Type{<:Domain}) = IndexCartesian()

@inline Base.haskey(d::Domain{T, N}, I::NTuple{N, Int}) where {T, N} = 
    all(in.(I, axes(d)))
@inline Base.checkbounds(d::Domain{T, N}, I::Vararg{Int, N}) where {T, N} =
    haskey(d, I) || throw(BoundsError(d, I))
@propagate_inbounds @inline function Base.getindex(d::Domain{T, N}, I::Vararg{Int, N}) where {T, N}
    @boundscheck checkbounds(d, I...)
    return d.data[index(d.topology, axes(d), I...)...]
end
@propagate_inbounds @inline function Base.setindex!(d::Domain{T, N}, x, I::Vararg{Int, N}) where {T, N}
    @boundscheck checkbounds(d, I...)
    return d.data[index(d.topology, axes(d), I...)...] = x
end



abstract type Stencil{N} end

# abstract interface
Base.length(::Type{T}) where T <: Stencil = prod(size(T))
@inline Base.isempty(::Type{T}) where T <: Stencil = length(T) == 0

@interface Base.size(T::Type{<:Stencil})
@interface Base.in(I::NTuple{N, Int}, T::Type{<:Stencil{N}}) where N
@interface Base.iterate(T::Type{<:Stencil})
@interface Base.iterate(T::Type{<:Stencil}, state)

# utility
@inline Base.checkbounds(::Type{T}, I::Vararg{Int, N}) where {N, T <: Stencil{N}} =
    I in T || throw(BoundsError(T, I))

function convert_stencil_args(args::Vararg{N}) where N
    f(x::AbstractUnitRange) = x
    f(x::Integer) = x:x
    f(x::Number) = f(Int(x))
    f(x::AbstractVector{<:Number}) = f(minimum(x)):f(maximum(x))
    f(x) = error("bad stencil argument: $x")

    return f.(args)
end

    
struct BoxStencil{N, I, S} <: Stencil{N}
    function BoxStencil(args::Vararg{N}) where N
        xs = convert_stencil_args(args...)
        I = Tuple{xs...}
        S = Tuple{length.(xs)...}
        return BoxStencil{N, I, S}
    end
end

# Stencil interface
@inline Base.size(::Type{BoxStencil{N, I, S}}) where {N, I, S} = tuple(S.parameters...)
@inline Base.axes(::Type{BoxStencil{N, I}}) where {N, I} = tuple(I.parameters...)
@inline Base.in(I::NTuple{N, Int}, ::Type{BoxStencil{N, J, S}}) where {N, J, S} =
    all(in.(I, J.parameters))
@inline Base.iterate(::Type{T}) where T <: BoxStencil = iterate(T, 1)
@inline function Base.iterate(::Type{T}, i::Int) where {N, I, S, T <: BoxStencil{N, I, S}}
    1 <= i <= length(T) || return nothing
    return getindex.(axes(T), Base._ind2sub(size(T), i))
end

# TODO: star stencil
# TODO: diamond stencil
# TODO: circle stencil



struct StencilView{T, N, D <: Domain{T, N}, S <: Stencil{N}} <: AbstractArray{T, N}
    domain::D
    I::NTuple{N, Int}

    function StencilView{S}(domain::D, I::Vararg{Int, N}) where {T, N, D <: Domain{T, N}, S}
        return new{T, N, D, S}(domain, I)
    end
end

# AbstractArray interface
@inline Base.size(::Type{StencilView{T, N, D, S}}) where {T, N, D, S} = size(S)
@inline Base.size(::T) where T <: StencilView = size(T)
@inline Base.IndexStyle(::Type{<:StencilView}) = IndexCartesian()
@propagate_inbounds @inline function Base.getindex(v::StencilView{T, N, D, S}, I::Vararg{Int, N}) where {T, N, D, S}
    @boundscheck checkbounds(S, I...)
    return v.domain[v.I .+ I]
end
@propagate_inbounds @inline function Base.setindex!(v::StencilView{T, N, D, S}, x, I::Vararg{Int, N}) where {T, N, D, S}
    @boundscheck checkbounds(S, I...)
    v.domain[v.I .+ I] = x
end

# custom iteration
@inline function Base.iterate(v::StencilView{T, N, D, S}) where {T, N, D, S}
    x = iterate(S)
    isnothing(x) && return nothing
    return iterate(v, @inbounds x[1])
end
@inline function Base.iterate(v::StencilView{T, N, D, S}, state) where {T, N, D, S}
    x = iterate(S, state)
    return @inbounds v[x[1]...], x[2]
end

# utility
function onborder(v::StencilView{T, N, D, S}) where {T, N, D, S}
    function issubset(a::AbstractUnitRange, b::AbstractUnitRange)
        return minimum(a) <= minimum(b) && maximum(a) >= maximum(b)
    end

    return !all(issubset.(axes(v.domain), axes(S)))
end


struct StencilIter{T, N, D <: Domain{T, N}, S <: Stencil{N}, V <: StencilView{T, N, D, S}} <: AbstractArray{V, N}
    domain::D

    function StencilIter{S}(domain::D) where {T, N, D <: Domain{T, N}, S}
        V = DomainView{T, N, D, S}
        return new{T, N, D, S, V}(domain)
    end
end
@inline Base.size(iter::StencilIter) = size(iter.domain)
@inline Base.IndexStyle(::Type{<:StencilIter}) = IndexCartesian()
@propagate_inbounds @inline function Base.getindex(iter::StencilIter{T, N, D, S, V}, I::Vararg{T, N}) where {T, N, D, S, V}
    @boundscheck checkbounds(iter.domain, I...)
    return V(iter.domain, I)
end



# utility
export clamped, periodic, mirror, onborder
@inline clamped(a::AbstractArray{T, N}, stencil::Vararg{N}) where {T, N} =
    StencilIter{BoxStencil(stencil...)}(Domain(a, Clamped{N}))
@inline periodic(a::AbstractArray{T, N}, stencil::Vararg{N}) where {T, N} =
    StencilIter{BoxStencil(stencil...)}(Domain(a, Periodic{N}))
@inline mirror(a::AbstractArray{T, N}, stencil::Vararg{N}) where {T, N} =
    StencilIter{BoxStencil(stencil...)}(Domain(a, Mirror{N}))

# NOTE: usage: dy = [0.5(x[0, 1] - x[0, -1]) for x in clamped(data, 0, -1:1) if !onborder(x)]