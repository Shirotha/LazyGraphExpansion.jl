using StaticArrays

export LVector
mutable struct LVector{N, T} <: AbstractVector{T}
    count::Int
    overflow::Int
    data::MMatrix{2, N, T}
    bucket::T

    function LVector{N, T}() where {N, T}
        @assert N isa Integer && N > 0 "N needs to be a positive integer!"
        @assert hasmethod(merge, (T, Int, T, Int)) "missing method merge(::$T, ::Int, ::$T, ::Int)!"
        new{N, T}(0, 0, zeros(MMatrix{2, N, T}), zero(T))
    end
end

# Constructors
function LVector(v::LVector)
    new = similar(v)
    new.count = v.count
    new.overflow = v.overflow
    new.data = copy(v.data)
    new.bucket = v.bucket
    return new
end
function LVector{N, T}(v::LVector{X, S}) where {N, T, X, S}
    new = LVector{N, T}()
    n = length(new.data)
    m = length(v.data)
    new.count = v.count
    if N < X
        error("shrinking a LVector is not implemented yet!")
        # FIXME: re-adjust for new overflow
        copyto!(new.data, 0, v.data, 0, )
        @inline f(i) = @fastmath 1 << (div(i - 1, 2) - N)
        x = 0
        for i in (n + 1):m
            x = f(i)
            new.bucket = merge(new.bucket, new.overflow, @inbounds v.data[i], x)
            new.overflow += x
        end
        x = f(m)
        new.bucket = merge(new.bucket, new.overflow, v.bucket, v.overflow * x)
        new.overflow += x
    elseif N > X
        # TODO: how to move overflow from v into new data? (set elements equal to bucket until all are split?)
        error("growing a LVector is not implemented yet!")
    else
        new.overflow = v.overflow
        new.data = convert(MVector{N, T}, v.data)
        new.bucket = v.bucket
    end
    return new
end
function LVector{N, T}(v::AbstractVector) where {N, T}
    new = LVector{N, T}()
    for x in v
        push!(new, x)
    end
    return new
end

# Conversion
Base.convert(::Type{LVector{N, T}}, v::LVector{N, T}) where {N, T} = v
Base.convert(::Type{LVector{N, T}}, v::AbstractVector) where {N, T} = LVector{N, T}(v)
Base.promote_rule(::Type{LVector{N, T}}, ::Type{LVector{X, S}}) where {N, T, X, S} = 
    LVector{max(N, X), promote_type(T, S)}

# AbstractVector interface
Base.size(v::LVector) = (v.count,)
@inline function Base.getindex(v::LVector{N}, i::Int) where N
    @boundscheck i <= 0 && throw(BoundsError(v, i))

    i = map_index(i, v.count, N)
    return i == 0 ? v.bucket : @inbounds v.data[i]
end
Base.setindex!(v::LVector, x, i::Int) = error("LVector can't be modified!")
Base.IndexStyle(::Type{<:LVector}) = IndexLinear()
Base.length(v::LVector) = v.count
Base.similar(v::LVector{N, T}) where {N, T} = LVector{N, T}() 
Base.similar(v::LVector{N}, ::Type{T}) where {N, T} = LVector{N, T}()
function Base.similar(::LVector, ::Type{T}, dims::Dims{D}) where {T, D}
    if D != 1
        return similar([], T, dims)
    else
        return LVector{@inbounds dims[1], T}()
    end
end


# Extensions
export capacity
@inline capacity(layer) = 2((1 << layer) - 1)
@inline capacity(v::LVector{N}) where N = capacity(N)
@inline function needshift(layer, count)
    @fastmath begin
        m = 1 << (layer - 1)
        x = count - (3m - 2)
        return x >= 0 && x % 2m == 0
    end
end
@inline function needcombine(layer, count)
    @fastmath begin
        m = 1 << layer
        x = count - (2m - 2)
        return x >= 0 && x % m == 0
    end
end
@inline function hasgap(layer, count)
    @fastmath begin
        m = 1 << layer
        x = count - (2m - 2)
        return x >= 0 && 1 <= x % m <= (m >> 1)
    end
end
function map_index(i::Int, count, layers)
    offset = 0
    last_offset = 0
    b0 = 1
    while true
        b = exponent(div(2(i + 1), 3))
        for l in b0:b
            hasgap(l, count) && (offset += 1 << (l - 1))
        end

        if offset == last_offset
            a = exponent(i + 1)
            a > layers && return 0

            return a + b
        end

        last_offset = offset
        b0 = b + 1
        i += offset
    end
end
function Base.push!(v::LVector{N, T}, x::T) where {N, T}
    for layer in N:-1:1
        if needcombine(layer, v.count)
            y = merge((@inbounds v.data[1, layer]), 1, (@inbounds v.data[2, layer]), 1)
            if layer == N
                v.overflow > 0 && (y = merge(y, 1, v.bucket, v.overflow))
                v.bucket = y
                v.overflow += 1
            else
                @inbounds v.data[1, layer + 1] = y
            end
        end
        if needshift(layer, v.count)
            @inbounds v.data[2, layer] = v.data[1, layer]
        end
    end
    @inbounds v.data[1] = x
    v.count += 1
    return v
end
Base.push!(v::LVector{N, T}, x) where {N, T} = push!(v, convert(T, x))
Base.push!(v::LVector{N, T}, xs...) where {N, T} = push!.((v,), xs)

(Base.merge(a::T, Na::Int, b::T, Nb::Int)::T) where T <: Number = (Na * a + Nb * b) / (Na + Nb)
Base.merge(a::T, Na::Int, b::S, Nb::Int) where {T <: Number, S <: Number} = let x = promote(a, b)
    merge(@inbounds x[1], Na, @inbounds x[2], Nb)
end

# Element-wise operations
# TODO: implement element-wise operations

# Iteration Utilities
export repeated
struct RepeatedIter{N, T}
    v::LVector{N, T}
end
@inline repeated(v::LVector) = RepeatedIter(v)
@inline Base.length(iter::RepeatedIter) = map_index(iter.v.count) + sign(v.overflow)
@inline Base.eltype(iter::RepeatedIter{N, T}) where {N, T} = Tuple{T, Int}
@inline Base.iterate(iter::RepeatedIter) = iterate(iter, (1, 1))
function Base.iterate(iter::RepeatedIter{N}, (l, i)::Tuple{Int, Int}) where N
    if l > N
        n = iter.v.overflow * (1 << N)
        return (iter.v.bucket, n), (-1, -1)
    elseif !(1 <= l <= N && 1 <= i <= 2)
        return nothing
    end

    x = iter.v.count - capacity(l - 1)
    x <= 0 && return nothing

    n = 1 << (l - 1)
    i == 2 && (x <= n || hasgap(l, iter.v.count)) && return iterate(iter, (l + 1, 1))

    return (@inbounds iter.v.data[i, l], n), (i == 2 ? (l + 1, 1) : (l, 2))
end

@inline Base.sum(v::LVector) = sum(s -> s[1] * s[2], repeated(v))
@inline Base.sum(f, v::LVector) = sum(s -> f(s[1]) * s[2], repeated(v))


# Printing
function Base.showarg(io::IO, v::LVector{N, T}, toplevel::Bool) where {N, T}
    if toplevel
        print(io, N, "-layer LVector{", N, ", ", T, "}")
    else
        print(io, "::LVector{", N, ", ", T, "}")
    end
end
Base.show(io::IO, v::LVector{N, T}) where {N, T} = print(io, "LVector{", N, ", ", T, "}(", v.data, "; ", v.bucket, ")")
function Base.show(io::IO, ::MIME"text/plain", v::LVector{N, T}) where {N, T}
    if get(io, :compact, false)
        show(io, v)
    else
        Base.showarg(io, v, true)
        println(io, ":")
        Base.print_array(io, v.data)
        println(io, v.bucket)
        println(io)
    end
end