module fuzzynum

primitive type FuzzyFloat <: AbstractFloat 64 end

FuzzyFloat(x :: Float64) = reinterpret(FuzzyFloat, x)

Float64(x :: FuzzyFloat) = reinterpret(Float64, x)

Base.show(io :: IO, x :: FuzzyFloat) = print(io, Float64(x))

import Base: +, *, zero

+ (a :: FuzzyFloat, b :: FuzzyFloat) = FuzzyFloat(max(Float64(a), Float64(b)))

*(a :: FuzzyFloat, b :: FuzzyFloat) = FuzzyFloat(min(Float64(a), Float64(b)))

zero(a :: FuzzyFloat) = reinterpret(FuzzyFloat, 0.0)

zero(a :: Type{FuzzyFloat}) = reinterpret(FuzzyFloat, 0.0)

export FuzzyFloat, +, *, zero

end
