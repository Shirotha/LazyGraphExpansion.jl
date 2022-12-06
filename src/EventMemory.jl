using DataStructures

mutable struct EventMemory{N, T}
    memory::RobinDict{Symbol, LVector{EventReport{T}}}
end

# Constructors
@inline EventMemory{N, T}() where {N, T} = 
    EventMemory{N, T}(RobinDict{Symbol, LVector{EventReport{T}}}())

# Extensions
function Base.push!(m::EventMemory{N, T}, type::Symbol, e::EventReport{T}) where {N, T}
    @boundscheck !haskey(m.memory, type) && (m.memory[type] = LVector{N, T}())
    push!(@inbounds m.memory[type], e)
    return m
end

# TODO: predict upcoming events based on known ones
#=
# NOTE: faster evaluation it reports is given as array of challenge and seperate array of (Δtime, Δenergy)
treat EventReport as a point of a curve with coordinates (Δtime, Δenergy) and parameter challenge
fit spline onto memory data (per type) by minimizing control points of f: c |-> (Δt, ΔE) over
function error(f, reports::AbstractVector{EventReport{T}}) where T
    data = reinterpret(reshape, T, reports)
    p = reinterpret(reshape, Vec{2, T}, @view data[1:2, :])
    c = @view data[3, :]

    err = sum(@. (f(c) - p)^2)
    return err[1], err[2]
end

determine local error using
function error(f, x, reports::AbstractVector{EventReport{T}}) where T
    data = reinterpret(reshape, T, reports)
    p = reinterpret(reshape, Vec{2, T}, @view data[1:2, :])
    c = @view data[3, :]

    w = @. exp(-(x - c)^2)

    err = sum(@. w * abs(f(c) - p)) / sum(w)
    return err[1], err[2]
end
maybe fit spline onto that for faster evaluation

update fits after each new data point with initial values from last fit
=#