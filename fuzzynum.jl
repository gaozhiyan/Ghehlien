module fuzzynum

primitive type FuzzyNum <: AbstractFloat 64 end

FuzzyNum(x :: Float64) = reinterpret(FuzzyNum, x)

Float64(x :: FuzzyNum) = reinterpret(Float64, x)

Base.show(io :: IO, x :: FuzzyNum) = print(io, Float64(x))

import Base: +

+ (a :: FuzzyNum, b :: FuzzyNum) = FuzzyNum(max(Float64(a), Float64(b)))

+ (a :: FuzzyNum, b :: Int64) = FuzzyNum(Float64(a) + b)

import Base: *

*(a :: FuzzyNum, b :: FuzzyNum) = FuzzyNum(min(Float64(a), Float64(b)))

import Base: zero

zero(a :: FuzzyNum) = reinterpret(FuzzyNum, 0.0)

zero(a :: Type{FuzzyNum}) = reinterpret(FuzzyNum, 0.0)

export FuzzyNum, +, *, zero

end
