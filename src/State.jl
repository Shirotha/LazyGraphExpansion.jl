export State
mutable struct State{N, T <: Real}
    time::T
    energy::T

    memory::EventMemory{N, T}

    State{N, T}(time, energy) where {N, T} = 
        new{N, T}(time, energy, EventMemory{N, T}())
end

@inline @propagate_inbounds Base.push!(s::State, c, Δt, ΔE) = push!(s, EventReport(c, Δt, ΔE))
function Base.push!(s::State, e::EventReport)
    s.time -= e.Δtime
    s.energy += e.Δenergy
    push!(memory, e)
    return s
end