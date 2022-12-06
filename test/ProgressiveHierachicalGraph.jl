@testset verbose=true "ProgressiveHierachicalGraph" begin
    let h = ProgressiveHierachicalGraph{Int}(1, 2)
        function subgraph_generator(g, p)
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
        function lock_generator(filter, lock!, p)
            @test p == [2 => 4, 2 => 3]
            @test filter(2 => 4) == [1, 8]
            @test filter(2 => 3) == [1, 5]
            @test filter(2 => 4; exclusive = true) == [8]

            lock!(8, 2 => 4)
            lock!(5, 2 => 3)
        end
        add_subgraph!(subgraph_generator, lock_generator, h, 4)

        @test locks(h) == [8, 5]
        @test keys(h, 4) == [1, 2]
        @test filter(h, 1) == [4, 5, 6, 7, 8]
    end
end