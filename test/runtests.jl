using Test
using LazyGraphExpansion

@testset verbose=true "Lazy Graph Expansion" begin

    include("util.jl")
    include("BiDict.jl")
    include("IndexedArray.jl")
    include("LVector.jl")

    include("DominatorTree.jl")
    include("PathData.jl")

    include("HierachicalGraph.jl")
    include("PathAwareHierachicalGraph.jl")
    include("ProgressiveHierachicalGraph.jl")

end