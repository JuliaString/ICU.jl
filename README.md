ICU.jl: International Components for Unicode (ICU) wrapper for Julia
====================================================================

Julia wrapper for the
[International Components for Unicode (ICU) libraries](http://site.icu-project.org/).

[![ICU](http://pkg.julialang.org/badges/ICU_0.5.svg)](http://pkg.julialang.org/detail/ICU)
[![ICU](http://pkg.julialang.org/badges/ICU_0.6.svg)](http://pkg.julialang.org/detail/ICU)

[![Build Status](https://travis-ci.org/JuliaString/ICU.jl.svg?branch=master)](https://travis-ci.org/JuliaString/ICU.jl)
[![Coverage Status](https://coveralls.io/repos/JuliaString/ICU.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/JuliaString/ICU.jl?branch=master)
[![codecov.io](http://codecov.io/github/JuliaString/ICU.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaString/ICU.jl?branch=master)

This is a new wrapper for the ICU library, designed to work on Julia v0.5
and above (however, since it depends on LegacyStrings, which is no longer being maintained,
it will no longer work after Julia v0.6).
The API has been redesigned to not pollute the namespace and to be a
bit more "Julian"
