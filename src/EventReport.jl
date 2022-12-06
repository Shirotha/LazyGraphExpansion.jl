struct EventReport{T <: Real}
    challenge::T
    Δtime::T
    Δenergy::T
end

# Constructors
@inline EventReport{T}(e::EventReport) where T =
    EventReport{T}(e.challene, e.Δtime, e.Δenergy)

# Conversion
@inline Base.convert(::Type{EventReport{T}}, e::EventReport) where T = EventReport{T}(e)
Base.promote_rule(::Type{EventReport{T}}, ::Type{EventReport{S}}) where {T, S} =
    EventReport{promote_type(T, S)}

# LVector Interface
@inline Base.merge(a::T, Na::Int, b::T, Nb::Int) where T <: EventReport =
    EventReport(
        merge(a.challenge, Na, b.challene, Nb),
        merge(a.Δtime, Na, b.Δtime, Nb),
        merge(a.Δenergy, Na, b.Δenergy, Nb)
    )