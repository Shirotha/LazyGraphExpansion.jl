@testset verbose=true "PathAwareHierachicalGraph" begin
    let h = PathAwareHierachicalGraph{Int}(1, 2)
        add_subgraph!(h, 4) do g, p
            @test entries(p) == [2]
            @test exits(p) == [3, 4]
            @test !isentry(p, 1)
            @test !isexit(p, 1)

            rem_edge!(g, 1, 3)
            rem_edge!(g, 1, 4)

            add_vertices!(g, 4)

            add_edge!(g, 1, 5)
            add_edge!(g, 5, 1)

            add_edge!(g, 1, 6)
            add_edge!(g, 6, 1)
            
            add_edge!(g, 6, 7)
            add_edge!(g, 7, 1)

            add_edge!(g, 5, 8)
            add_edge!(g, 8, 5)

            add_edge!(g, 7, 8)
            add_edge!(g, 8, 7)

            add_edge!(g, 5, 3)
            add_edge!(g, 8, 4)
        end

        add_subgraph!(h, 8) do g, p
            @test entries(p) == [2, 3]
            @test exits(p) == [2, 3, 4]
            # NOTE 2 => 4 are parent vertices, not interface vertices from this sub-graph
            @test paths(p) == [2 => 4]
            @test entries(p, 2 => 4) == [2, 3]
            @test exits(p, 2 => 4) == [4]
        end
    end
end