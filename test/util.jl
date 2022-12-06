@testset "Maybe" begin

    @test maybe(first, (1, 2)) == 1
    @test maybe(first, nothing) == nothing

    let x::Maybe{Int} = 1
        @maybe x x = nothing
        @test x == nothing

        @maybe x x = 2
        @test x == nothing
    end
end

@testset "Array Utils" begin
    let a = [1, 2, 3, 4]
        @test first(iseven, a) == 2
        @test first(>(4), a) == nothing

        deletefirst!(iseven, a)
        @test a == [1, 3, 4]
    end
end