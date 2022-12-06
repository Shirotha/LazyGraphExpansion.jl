using LazyGraphExpansion, Random, Graphs

LGE = LazyGraphExpansion

mutable struct MazeGenerator{RNG <: AbstractRNG} <: GraphGeneratorFactory
    rng::RNG
end

# Constructors
function MazeGenerator(; rng=Random.GLOBAL_RNG)
    return MazeGenerator(rng)
end

# GraphGeneratorFactory interface
function LGE.subgraph_generator(gen::MazeGenerator, state::State)
    function result(g, d)
        function required(p)
            function star()
                N0 = nv(g)
                add_vertices!(g, N0 - 1)
                N = nv(g)

                for v in 2:N0
                    w = v + N0 - 1
                    vw = isentry(d, v)
                    wv = isexit(d, v)

                    bi(mov_edge!, g, v, 1 => w, vw, wv)
                    bi(add_edge!, g, w, 1, vw, wv)
                    
                    # TODO: chance to add opposite connection between (1, w), even when not required
                end
            end
            function choice(n)
                @assert n >= 1 "need at least one path!"
                N0 = nv(g)
                add_vertices!(g, n + 1)
                N = nv(g)
                for v in (N0 + 1):(N - 1)
                    bi(add_edge!, g, 1, v)
                    bi(add_edge!, g, v, N)
                end

                for v in exits(d, p)
                    bi(mov_edge!, g, v, 1 => N, isentry(d, v), isexit(d, v))
                end
            end

            n = rand(gen.rng, 1:2)
            if n == 1
                star()
            else
                choice(rand(gen.rng, 2:3))
            end

            interface = union(entries(d), exits(d))
            inner = filter(!in(interface), vertices(g))
            while rand(gen.rng, Bool)
                add_vertex!(g)
                N = nv(g)
                v = rand(gen.rng, inner)

                bi(add_edge!, g, v, N)
            end
        end
        function optional()
            function circle()
                N0 = nv(g)
                add_vertices!(g, N0 - 2)
                N = nv(g)

                for v in 2:(N0 - 1)
                    w = v + N0 - 1

                    bi(mov_edge!, g, v, 1 => w, isentry(d, v), isexit(d, v))
                end

                for v in (N0 + 1):(N - 1)
                    bi(add_edge!, g, v, v + 1)
                end

                bi(add_edge!, g, N0 + 1, 1)

                N0 + 1 == N && return

                bi(add_edge!, g, N, 1)
            end
            function trap()
                add_vertex!(g)
                N = nv(g)

                mov_edge!.((g,), 1 => N, exits(d))

                add_edge!(g, 1, N)
                rand(gen.rng, Bool) && add_edge!(g, N, 1)
            end
            
            n = rand(gen.rng, 1:2)
            if n == 1
                circle()
            else
                trap()
            end
        end

        ps = paths(d)
        if isempty(ps)
            optional()
        else
            p = rand(gen.rng, ps)
            required(p)
        end
    end

    return result
end

function LGE.lock_generator(gen::MazeGenerator, state::State)
    (f, l!, ps) -> nothing
end

function LGE.should_expand(gen::MazeGenerator, g::LayoutGraph, v::Int, state::State)
    ispinned(g, v) && return false
    
    h = g.graph.graph.graph
    vertex = h.vertices[v]
    path = LazyGraphExpansion.tree_path(h.hierachy, 1, vertex.subgraph)

    @assert !isnothing(path) "no path to root found!?"
    length(path) <= 3 && return true

    pin!(g, v)
    return false
end