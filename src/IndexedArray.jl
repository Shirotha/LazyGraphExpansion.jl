using DataStructures
using Base:@propagate_inbounds

export IndexedArray
mutable struct IndexedArray{T, N} <: AbstractArray{T, N}
    data::Array{T, N}
    index::RobinDict{T, Int}
end

# Constructors
function IndexedArray{T, N}(args...) where {T, N}
    data = Array{T, N}(args...)
    index = RobinDict{T, Int}()
    for (i, x) in enumerate(data)
        @assert !haskey(index, x) "duplicate elements not allowed!"
        index[x] = i
    end
    IndexedArray{T, N}(data, index)
end
@inline IndexedArray{T, N}(a::IndexedArray) where {T, N} = 
    IndexedArray{T, N}(Array{T, N}(a.data), RobinDict{T, Int}(a.index))

# Conversion
@inline Base.convert(::Type{IndexedArray{T, N}}, a::IndexedArray{T, N}) where {T, N} = a
@inline Base.convert(::Type{IndexedArray{T, N}}, a::AbstractArray) where {T, N} = 
    IndexedArray{T, N}(a)

# Promotion
@inline Base.promote_rule(::Type{IndexedArray{T, N}}, ::Type{IndexedArray{S, N}}) where {T, S, N} =
    IndexedArray{promote_type(T, S), N}

# Aliases
export IndexedVector, IndexedMatrix
const IndexedVector{T} = IndexedArray{T, 1}
const IndexedMatrix{T} = IndexedArray{T, 2}

# Array Interface
@inline Base.size(a::IndexedArray) = size(a.data)
@inline Base.IndexStyle(::Type{<:IndexedArray}) = IndexLinear()
@inline @propagate_inbounds Base.getindex(a::IndexedArray, i::Int) = a.data[i]
@propagate_inbounds function Base.setindex!(a::IndexedArray, x, i::Int)
    delete!(a.index, a.data[i])
    a.data[i] = x
    a.index[x] = i
    return x
end
@inline Base.iterate(a::IndexedArray) = iterate(a.data)
@inline Base.iterate(a::IndexedArray, s) = iterate(a.data, s)
function Base.push!(a::IndexedArray, x)
    @assert !haskey(a.index, x) "duplicate elements not allowed!"
    push!(a.data, x)
    a.index[x] = length(a.data)
    return a
end
function Base.deleteat!(a::IndexedArray, i)
    delete!(a.index, a.data[i])
    deleteat!(a.data, i)
    return a
end
function Base.delete!(a::IndexedArray, x)
    deleteat!(a.data, a.index[x])
    delete!(a.index, x)
    return a
end
@inline Base.in(x, a::IndexedArray) = haskey(a.index, x)

# Index lookup
export index, try_index
@inline @propagate_inbounds index(a::IndexedArray, x) = a.index[x]
@inline try_index(a::IndexedArray, x) = x in a ? a.index[x] : nothing