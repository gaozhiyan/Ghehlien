module fuzzynum

############ Fuzzy Numbers ############

import Base: +, *, zero

primitive type FuzzyFloat <: AbstractFloat 64 end

FuzzyFloat(x :: Float64) = reinterpret(FuzzyFloat, x)
Float64(x :: FuzzyFloat) = reinterpret(Float64, x)
Base.show(io :: IO, x :: FuzzyFloat) = print(io, Float64(x))
+(a :: FuzzyFloat, b :: FuzzyFloat) = FuzzyFloat(max(Float64(a), Float64(b)))
*(a :: FuzzyFloat, b :: FuzzyFloat) = FuzzyFloat(min(Float64(a), Float64(b)))
zero(a :: FuzzyFloat) = reinterpret(FuzzyFloat, 0.0)
zero(a :: Type{FuzzyFloat}) = reinterpret(FuzzyFloat, 0.0)

export FuzzyFloat, +, *, zero

############ Array Utilities ############

function setToList(s)
    dupS = copy(s)
    n = length(dupS)
    xs = []
    for i in 1:n
        push!(xs, pop!(dupS))
    end
    xs
end

function getIndexInArr(xs, k)
    len = length(xs)
    for i in 1:len
        if k == xs[i]
            return i
        end
    end
    return -1
end

export setToList, getIndexInArr

############ Ghehlien (Plain Binary Relation) ############

function getTransitiveClosure(FuzzyArr)
    while true
        arr_new = FuzzyArr * FuzzyArr
        if arr_new == FuzzyArr
            return arr_new
        end
        FuzzyArr = arr_new
    end
end

function getCluster(arr, n)
    arr_new = map(x -> Float64(x), arr)

    ss = []
    for i in 1:n
        if all(xs -> !in(i, xs), ss)
            s = map(x -> x[2], filter(x -> x[1] != zero(x[1]), zip(view(arr_new, :, i), collect(1:n))))
            if length(s) != 0
                push!(ss, s)
            end
        end
    end
    ss
end

function ghehlien(xs)
    xsLen = length(xs)
    xsUniqueSet = Set(map(x -> x[1], xs))
    union!(xsUniqueSet, Set(map(x -> x[2], xs)))
    xsUniqueLen = length(xsUniqueSet)
    xsArr = eye(Bool, xsUniqueLen)
    xsUniqueList = setToList(xsUniqueSet)
    for i in 1:xsLen
        i1 = getIndexInArr(xsUniqueList, xs[i][1])
        i2 = getIndexInArr(xsUniqueList, xs[i][2])
        xsArr[i1, i2] = true
        xsArr[i2, i1] = true
    end
    fuzzyArr = map(x -> FuzzyFloat(x ? 1.0 : 0.0), xsArr)
    fuzzyTransitiveClosure = getTransitiveClosure(fuzzyArr)
    ss = getCluster(fuzzyTransitiveClosure, xsLen)
    for i in map(m -> join(map(n -> xsUniqueList[n], m)), ss)
        println(i)
    end
end

export ghehlien

end
