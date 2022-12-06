@testset verbose=true "IndexedArray" begin
    let a = IndexedVector{Int}(2:4)
        @test length(a) == 3

        a[2] = 5
        @test a[2] == 5

        push!(a, 6)
        @test a[4] == 6

        @test collect(a) == [2, 5, 4, 6]
        let i = index(a, 5)
            @test a[i] == 5
            deleteat!(a, i)
            @test a == [2, 4, 6]
        end
    end
end