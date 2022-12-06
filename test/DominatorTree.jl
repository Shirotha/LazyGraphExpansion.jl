using Graphs
using LazyGraphExpansion: dominator_tree
@testset verbose=true "Dominator Tree" begin
    let g = SimpleDiGraph{Int}(5)
        add_edge!(g, 1, 2)
        add_edge!(g, 1, 3)
        add_edge!(g, 3, 4)
        add_edge!(g, 4, 1)
        add_edge!(g, 2, 5)
        add_edge!(g, 4, 5)

        let d = dominator_tree(g, 1)
            @test nv(d) == 5
            @test ne(d) == 4

            @test has_edge(d, 1, 2)
            @test has_edge(d, 1, 3)
            @test has_edge(d, 1, 5)
            @test has_edge(d, 3, 4)
        end
    end
end