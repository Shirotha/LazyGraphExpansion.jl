export VarTuple
VarTuple{T} = Tuple{Vararg{T}}

export Maybe
Maybe{T} = Union{Nothing, T}

export maybe, @maybe
maybe(f::Function, x::Maybe) = isnothing(x) ? nothing : f(x)
macro maybe(x, f)
    :(isnothing($(esc(x))) || $(esc(f)))
end

export deletefirst!
Base.first(f::Function, a::AbstractArray) = maybe(i -> a[i], findfirst(f, a))
deletefirst!(f::Function, a::AbstractArray) = maybe(i -> deleteat!(a, i), findfirst(f, a))

macro interface(f)
    if f isa Symbol
        throw(ArgumentError("unexpected expression $f, expected call"))
    else # f isa Expr
        if f.head == :where
            g = f.args[1]
            g.head == :call || throw(ArgumentError("unexpected expression $g, expected call"))
            args = g.args[2:end]
        else
            f.head == :call || throw(ArgumentError("unexpected expression $f, expected call"))
            args = f.args[2:end]
        end
        
        return Expr(:(=), f, :(throw(MethodError($(args...)))))
    end
end