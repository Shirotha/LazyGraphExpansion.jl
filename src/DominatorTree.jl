using DataStructures, Graphs

function lengauer_tarjan(g::AbstractGraph{T}, root::T) where T
    num = nv(g)
    @assert 1 <= root <= num "root vertex out of bounds!"
    N = 1
    
    bucket = [Set{T}() for _ in 1:num]
    dfnum = zeros(Int, num)
    #=
    vertex = zeros(T, num)
    parent = zeros(T, num)
    semi = zeros(T, num)
    ancestor = zeros(T, num)
    idom = zeros(T, num)
    samedom = zeros(T, num)
    best = zeros(T, num)
    =#
    data = zeros(T, num, 7)
    vertex = @view data[:, 1]
    parent = @view data[:, 2]
    semi = @view data[:, 3]
    ancestor = @view data[:, 4]
    idom = @view data[:, 5]
    samedom = @view data[:, 6]
    best = @view data[:, 7]
    
    function dfs()
        stack = Stack{Tuple{T, T}}()
        push!(stack, (0, root))
        @inbounds while !isempty(stack)
            (p, n) = pop!(stack)
            dfnum[n] != 0 && continue

            dfnum[n] = N
            vertex[N] = n
            parent[n] = p
            N += 1

            for w in outneighbors(g, n)
                push!(stack, (n, w))
            end
        end
        return nothing
    end

    # TODO: unwind recursion
    function ancestor_with_lowest_semi(v::T)
        @inbounds begin
            a = ancestor[v]
            if ancestor[a] > 0
                b = ancestor_with_lowest_semi(a)
                ancestor[v] = ancestor[a]
                if dfnum[semi[b]] < dfnum[semi[best[v]]]
                    best[v] = b
                end
            end
            return best[v]
        end
    end

    function link(p::T, n::T)
        @inbounds ancestor[n] = p
        @inbounds best[n] = n
        return nothing
    end

    dfs()

    @inbounds for i in num:-1:1
        n = vertex[i]
        # NOTE: unreachable from root
        n == 0 && continue
        n == root && continue
        p = parent[n]
        s = p

        for v in inneighbors(g, n)
            # NOTE: unreachable from root
            dfnum[v] == 0 && continue
            sPrime = dfnum[v] <= dfnum[n] ? v : semi[ancestor_with_lowest_semi(v)]
            if dfnum[sPrime] < dfnum[s]
                s = sPrime
            end
        end

        semi[n] = s
        push!(bucket[s], n)
        link(p, n)

        for v in bucket[p]
            y = ancestor_with_lowest_semi(v)
            if semi[y] == semi[v]
                idom[v] = p
            else
                samedom[v] = y
            end
        end

        empty!(bucket[p])
    end

    @inbounds for i in 1:num
        i == root && continue
        n = vertex[i]
        if n > 0 && samedom[n] >= 1
            idom[n] = idom[samedom[n]]
        end
    end

    return idom
end

function dominator_tree(g::AbstractGraph{T}, root::T) where T
    idom = lengauer_tarjan(g, root)
    return Graphs.tree(idom)
end