using Graphs
using LazyGraphExpansion: PathData, display_paths
@testset verbose=true "PathData" begin
    let g = SimpleDiGraph{Int}(8)
        add_edge!(g, 8, 1)

        add_edge!(g, 1, 2)
        add_edge!(g, 2, 1)

        add_edge!(g, 1, 3)
        add_edge!(g, 3, 1)

        add_edge!(g, 3, 4)
        add_edge!(g, 4, 1)

        add_edge!(g, 2, 5)
        add_edge!(g, 5, 2)

        add_edge!(g, 4, 5)
        add_edge!(g, 5, 4)

        add_edge!(g, 2, 6)
        add_edge!(g, 5, 7)
       
        let p = PathData(g, [8], [6, 7])
            @test paths(p) == [8 => 6, 8 => 7]
            @test p[1, 8 => 6]
            @test p[1, 8 => 7]
            @test filter(p, 8 => 6) == [1, 2, 6, 8]
            @test filter(p, 8 => 6, 8 => 7) == [1, 8]
            @test collect(p[5, 8 => 7, :out]) == [findfirst(==(7), outneighbors(g, 5))]
        end
    end
end