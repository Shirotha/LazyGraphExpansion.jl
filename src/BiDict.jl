using DataStructures
using Base:@propagate_inbounds

export BiDict
mutable struct BiDict{TK, TV} <: AbstractDict{TK, TV}
    k2v::RobinDict{TK, TV}
    v2k::RobinDict{TV, TK}
end

# Constructors
function BiDict{TK, TV}() where {TK, TV}
    k2v = RobinDict{TK, TV}()
    v2k = RobinDict{TV, TK}()
    BiDict(k2v, v2k)
end
function BiDict{TK, TV}(b::BiDict) where {TK, TV}
    k2v = RobinDict{TK, TV}(b.k2v)
    v2k = RobinDict{TV, TK}(b.v2k)
    BiDict(k2v, v2k)
end
function BiDict(d::AbstractDict{TK, TV}) where {TK, TV}
    b = BiDict{TK, TV}()
    for (k, v) in d
        b[k] = v
    end
    return b
end
function BiDict{TK, TV}(d::AbstractDict) where {TK, TV}
    b = BiDict{TK, TV}()
    for (k, v) in d
        b[k] = v
    end
    return b
end

# Conversion
# NOTE: needed to prevent copying
@inline Base.convert(::Type{BiDict{TK, TV}}, b::BiDict{TK, TV}) where {TK, TV} = b
@inline Base.convert(::Type{Dict{TK, TV}}, b::BiDict) where {TK, TV} = Dict{TK, TV}(b.k2v)
@inline Base.convert(::Type{BiDict{TK, TV}}, d::AbstractDict) where {TK, TV} = BiDict{TK, TV}(d)

# Promotion
@inline Base.promote_rule(::Type{BiDict{TK, TV}}, ::Type{BiDict{SK, SV}}) where {TK, TV, SK, SV} =
    BiDict{promote_type(TK, SK), promote_type(TV, SV)}

# Iterator Interface
@inline Base.eltype(::BiDict{TK, TV}) where {TK, TV} = Pair{TK, TV}
@inline Base.length(b::BiDict) = length(b.k2v)
@inline Base.iterate(b::BiDict) = iterate(b.k2v)
@inline Base.iterate(b::BiDict, s) = iterate(b.k2v, s)

# Dict Interface
@inline @propagate_inbounds Base.getindex(b::BiDict, k) = b.k2v[k]
@inline @propagate_inbounds Base.getindex(b::BiDict, ::typeof(!), v) = b.v2k[v]
@inline @propagate_inbounds function Base.setindex!(b::BiDict, v, k)
    haskey(b.k2v, k) && delete!(b.v2k, b.k2v[k])
    b.k2v[k] = v
    b.v2k[v] = k
    return v
end
@inline @propagate_inbounds function Base.setindex!(b::BiDict, k, ::typeof(!), v)
    haskey(b.v2k, v) && delete!(b.k2v, b.v2k[v])
    b.v2k[v] = k
    b.k2v[k] = v
    return k
end
@inline Base.haskey(b::BiDict, k) = haskey(b.k2v, k)
export hasvalue
@inline hasvalue(b::BiDict, v) = haskey(b.v2k, v)
@inline Base.keys(b::BiDict) = keys(b.k2v)
@inline Base.values(b::BiDict) = values(b.k2v)