# TODO: how to handle crossings while claiming
#=
generate maze onto square grid (walls on edges)
dynamically growing matrix for maze data
when generating a vertex:
    run layout algorithm for graph (forcing interface vertices to outer edge; rest edge length ~ cell size in squares)
    pin vertex at current location (no change in position in future layout iterations)
    claim squares around vertex location using floodfill
        prioritize going towards neighboring vertices (or pinned vertices) (or edges between pinned vertices)
        discourage going towards un-pinned vertices (or unconnected vertices) (or edges between un-pinned vertices)
        mark square along the border to neighbors as connecton (prioritize going towards connections of neighboring vertices in future iterations)
        fill out pinned triangles (build by three neighboring vertices that are all pinned)
        fill out area bordering already claimed squares
    generate maze inside claimed area
=#

using GraphPlot, Compose, DataStructures, ProgressBars

include("LayoutGraph.jl")
include("MazeGenerator.jl")

function eager_explore(g, gen, state)
    openlist = Stack{Tuple{Int, Int}}()
    for v in entries(g)
        push!(openlist, (v, outneighbors(g, v)[1]))
    end

    while !isempty(openlist)
        pos, dest = pop!(openlist)
        ispinned(g, dest) && continue
        isexit(g, dest) && continue
        
        pos = move(g, pos, dest, gen, state)

        for v in outneighbors(g, pos)
            push!(openlist, (pos, v))
        end
    end

    return nothing
end

function plot(g)
    coords = reinterpret(reshape, Float64, g.positions)
    N = size(coords, 2)
    xs = coords[1, 1:N]
    ys = coords[2, 1:N]
    
    nodelabel = string.(1:N)
    nodefillc = fill("cyan", N)
    for v in entries(g)
        nodelabel[v] = "In $(entry_index(g, v))"
        nodefillc[v] = "light green"
    end
    for v in exits(g)
        nodelabel[v] = "Out $(exit_index(g, v))"
        nodefillc[v] = "pink"
    end
    
    dx = maximum(xs) - minimum(xs)
    dy = maximum(ys) - minimum(ys)
    aspect = dx / dy

    return gplot(g, xs, ys; nodelabel, nodefillc, 
        NODESIZE = 0.2 / N ^ (1 - 0.5exp(-0.5(aspect - 1) ^ 2)),
        arrowlengthfrac = 0.02, 
        arrowangleoffset = π/12), aspect
end

const SAVE_PATH = "examples/plots"
function save(g, name...)
    mkpath(SAVE_PATH)
    p, aspect = plot(g)
    out_path = joinpath(SAVE_PATH, string(name..., ".svg"))
    draw(SVG(out_path, aspect * 16cm, 16cm), p)
    return out_path
end

function compose(callback, N, name...)
    iter = ProgressBar(1:N)
    open(joinpath(SAVE_PATH, string(name..., ".html")), "w") do html
        index = 0

        function yielder(g, gen, state)
            eager_explore(g, gen, state)
        
            fn = save(g, name..., "-", index)
            fn = abspath(fn)

            println(html, "    <img src=\"file://", fn, "\" />")
        end

        println(html, "<!DOCTYPE html>")
        println(html, "<html>")
        println(html, "  <head><title>", name..., "</title></head>")
        println(html, "  <body>")

        for i in iter
            index = i
            callback(yielder)
        end

        println(html, "  </body>")
        println(html, "</html>")
    end
end


function main(N)
    compose(N, "test") do yield
        g = LayoutGraph{Float64, Int}(1, 1)
        gen = MazeGenerator()
        state = State{3, Float64}(1, 1)

        yield(g, gen, state)
    end
end

main(10)