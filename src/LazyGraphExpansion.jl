module LazyGraphExpansion

    # TODO: include doc templates

    include("util.jl")
    include("BiDict.jl")
    include("IndexedArray.jl")
    include("LVector.jl")

    include("DominatorTree.jl")
    include("PathData.jl")
    
    include("HierachicalGraph.jl")
    include("PathAwareHierachicalGraph.jl")
    include("ProgressiveHierachicalGraph.jl")

    include("EventReport.jl")
    include("EventMemory.jl")
    include("State.jl")

    include("GraphGeneratorFactory.jl")

    include("interface/Domain.jl")
    include("interface/graph-util.jl")
end