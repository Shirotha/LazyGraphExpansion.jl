# Helpers
function tree_path(g::AbstractGraph{T}, a::T, b::T)::Maybe{Vector{T}} where T
    path = Vector{T}()
    push!(path, b)
    while b != a
        ins = inneighbors(g, b)
        @assert length(ins) <= 1 "graph is not a valid tree!"
        isempty(ins) && return nothing
        b = @inbounds ins[1]
        push!(path, b)
    end
    reverse!(path)
    return path
end

function is_ancestor(g::AbstractGraph{T}, a::T, b::T) where T
    while a != b
        ins = inneighbors(g, b)
        @assert length(ins) <= 1 "graph is not a valid tree!"
        isempty(ins) && return false
        b = @inbounds ins[1]
    end
    return true
end



# Vertex Data
struct PathVertexData{T}
    paths::BitSet
    ins_after::Vector{Maybe{BitSet}}
    outs_after::Vector{Maybe{BitSet}}
end



# Path Data
struct PathData{T}
    paths::IndexedVector{Pair{T, T}}
    vertex_data::Vector{PathVertexData{T}}
end

# Constructors
struct _PathData{T}
    path::IndexedVector{T}
    ins_after::Vector{BitSet}
    outs_after::Vector{BitSet}
end
@inline PathData(g::AbstractGraph, entries, exits) = PathData(g, collect(entries), collect(exits))
function PathData(g::AbstractGraph{T}, entries::AbstractArray{T}, exits::AbstractArray{T}) where T
    data = Vector{_PathData{T}}()
    paths = IndexedVector{Pair{T, T}}()
    for a in entries
        tree = dominator_tree(g, a)
        for b in exits
            push!(paths, a => b)
            path = tree_path(tree, a, b)
            @assert !isnothing(path) "no path from vertex $a to vertex $b found!"
            ins_after = Vector{BitSet}(undef, length(path))
            outs_after = Vector{BitSet}(undef, length(path))
            for (i, v) in enumerate(path)
                @inbounds ins_after[i] = BitSet(findall(w -> is_ancestor(tree, v, w), inneighbors(g, v)))
                @inbounds outs_after[i] = BitSet(findall(w -> is_ancestor(tree, v, w), outneighbors(g, v)))
            end
            push!(data, _PathData{T}(path, ins_after, outs_after))
        end
    end

    vertex_data = Vector{PathVertexData{T}}(undef, nv(g))
    for v in vertices(g)
        on_path = BitSet()
        ins = Vector{Maybe{BitSet}}(nothing, length(data))
        outs = Vector{Maybe{BitSet}}(nothing, length(data))
        for (i, d) in enumerate(data)
            maybe(findfirst(==(v), d.path)) do j
                push!(on_path, i)
                ins[i] = d.ins_after[j]
                outs[i] = d.outs_after[j]
            end
        end
        @inbounds vertex_data[v] = PathVertexData{T}(on_path, ins, outs)
    end
    
    return PathData{T}(paths, vertex_data)
end

# Extensions
export paths
paths(d::PathData) = d.paths.data

function Base.filter(d::PathData{T}, paths::Vararg{Pair{T, T}, N}; exclusive::Bool=false) where {T, N}
    is = index.((d.paths,), paths)
    return findall(d.vertex_data) do v
        all(in(v.paths), is) && (!exclusive || length(is) == length(v.paths))
    end
end

@propagate_inbounds function Base.getindex(d::PathData{T}, v::T) where T
    return getindex.((d.paths,), d.vertex_data[v].paths)
end

@propagate_inbounds function Base.getindex(d::PathData{T}, v::T, p::Pair{T, T}) where T
    i = index(d.paths, p)
    return i in d.vertex_data[v].paths
end

@propagate_inbounds function Base.getindex(d::PathData{T}, v::T, p::Pair{T, T}, s::Symbol) where T
    i = index(d.paths, p)
    if s == :in
        return d.vertex_data[v].ins_after[i]
    elseif s == :out
        return d.vertex_data[v].outs_after[i]
    else
        error("unknown property")
    end
end

# Printing
function Base.show(io::IO, pd::PathData{T}) where T
    print(io, "PathData{$T}(", join(pd.paths, ", "), "; ")
    isfirst = true
    for vd in pd.vertex_data
        if isfirst
            isfirst = false
        else
            print(io, ", ")
        end
        print(io, "(")
        isfirst2 = true
        for p in vd.paths
            if isfirst2
                isfirst2 = false
            else
                print(io, ", ")
            end
            print(io, pd.paths[p], ": (", collect(vd.ins_after[p]), ", ", collect(vd.outs_after[p]), ")")
        end
        print(io, ")")
    end
    print(io, ")")
end
function Base.show(io::IO, ::MIME"text/plain", pd::PathData{T}) where T
    if get(io, :compact, false)
        show(io, pd)
    else
        println(io, "PathData{$T}")
        for (v, vd) in enumerate(pd.vertex_data)
            header = "[$v]: "
            print(io, header)
            padding = 0
            for p in vd.paths
                print(io, repeat(" ", padding))
                println(io, pd.paths[p], ": ", collect(vd.ins_after[p]), ", ", collect(vd.outs_after[p]))
                padding = length(header)
            end
        end
    end
end
function show_paths(io::IO, pd::PathData{T}, g::AbstractGraph{T}) where T
    if get(io, :compact, false)
        show(io, pd)
    else
        show(io, MIME"text/plain"(), g)
        println(io, "Paths:")
        for (v, vd) in enumerate(pd.vertex_data)
            isempty(vd.paths) && continue
            header = "[$v]: "
            print(io, header)
            padding = 0
            ins = inneighbors(g, v)
            outs = outneighbors(g, v)
            for p in vd.paths
                print(io, repeat(" ", padding))
                ins_after = getindex.((ins,), vd.ins_after[p])
                outs_after = getindex.((outs,), vd.outs_after[p])
                println(io, pd.paths[p], ": ", ins_after, ", ", outs_after)
                padding = length(header)
            end
        end
    end
end
display_paths(pd::PathData, g::AbstractGraph) = show_paths(stdout, pd, g)