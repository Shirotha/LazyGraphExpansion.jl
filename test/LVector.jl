@testset verbose=true "LVector" begin
    let v = LVector{3, Float64}(), xs = rand(Float64, 20)
        @test capacity(v) == 14
        
        push!(v, xs[1], xs[2])
        @test length(v) == 2
        @test v[2] == xs[1]
        
        push!(v, xs[3])
        @test v[1] == xs[3]
        let x = (xs[1] + xs[2]) / 2
            @test v[2] ≈ x
            @test v[3] ≈ x
        end

        push!(v, xs[4:20]...)
        @test sum(v) ≈ sum(xs)
    end
end