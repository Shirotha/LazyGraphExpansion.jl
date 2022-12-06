@testset verbose=true "BiDict" begin
    let d = BiDict{Int, Int}()
        @test eltype(d) == Pair{Int, Int}
        @test length(d) == 0

        d[1] = 2
        @test length(d) == 1
        @test d[1] == 2
        @test d[!, 2] == 1

        d[!, 2] = 3
        @test length(d) == 1
        @test d[!, 2] == 3
        @test d[3] == 2

        @test collect(d) == [3 => 2]
        @test haskey(d, 3)
        @test !haskey(d, 2)
        @test hasvalue(d, 2)
        @test !hasvalue(d, 3)
    end
end