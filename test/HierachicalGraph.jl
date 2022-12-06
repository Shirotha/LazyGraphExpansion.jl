using Graphs

@testset verbose=true "HierachicalGraph" begin
    let h = HierachicalGraph{Int}(1, 2)
        @testset "Entries and Exits" begin
            @test entries(h) == 1:1
            @test exits(h) == 2:3
            @test isentry(h, 1)
            @test !isentry(h, 2)
            @test isexit(h, 2)
            @test !isexit(h, 1)
            @test entry_index(h, 1) == 1
            @test entry_index(h, 2) == nothing
            @test exit_index(h, 2) == 1
            @test exit_index(h, 1) == nothing
        end

        @testset "AbstractGraph interface" begin
            @test nv(h) == 4
            @test vertices(h) == 1:4
            @test has_vertex(h, 1)
            @test !has_vertex(h, 5)

            @test ne(h) == 3
            @test collect(edges(h)) == [Edge(1, 4), Edge(4, 2), Edge(4, 3)]
            @test has_edge(h, 1, 4)
            @test !has_edge(h, 4, 1)

            @test inneighbors(h, 4) == [1]
            @test outneighbors(h, 4) == [2, 3]
        end

        @testset verbose=true "Subgraphs" begin
            @testset "Initialization" begin
                add_subgraph!(h, 4) do g
                    @test nv(g) == 4
                    @test collect(edges(g)) == [Edge(1, 3), Edge(1, 4), Edge(2, 1)]
                    
                    add_vertex!(g)

                    rem_edge!(g, 2, 1)
                    add_edge!(g, 2, 5)

                    add_edge!(g, 5, 1)
                    add_edge!(g, 1, 5)
                end
            end

            @testset "AbstractGraph interface" begin
                @test nv(h) == 5
                @test collect(edges(h)) == [Edge(1, 5), Edge(4, 2), Edge(4, 3), Edge(4, 5), Edge(5, 4)]

                @test inneighbors(h, 5) == [4, 1]
                @test outneighbors(h, 5) == [4]
            end
        end
    end
end