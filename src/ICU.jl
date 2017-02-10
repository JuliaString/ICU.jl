# ICU.jl - Wrapper for ICU (International Components for Unicode) library

# Some content of the documentation strings was derived from the ICU header files.
# (Those portions copyright (C) 1996-2015, International Business Machines Corporation and others)

"""
    ICU (International Components for Unicode) Wrapper
"""
module ICU

import Base: parse, get, close

export set_locale!

include("../deps/deps.jl")
include("../deps/versions.jl")
include("compat5.jl")

@static if is_windows()
    # make sure versions match
    v1 = last(matchall(r"\d{2}", iculib))
    v2 = last(matchall(r"\d{2}", iculibi18n))
    v1 == v2 ||
        error("ICU library version mismatch $v1 != $v2 -- please correct $(realpath("../deps/deps.jl"))")
end

function __init__()
    set_locale!("")
end

global version
global suffix

dliculib = Libdl.dlopen(iculib)

for (suf,ver) in [("",0);
                         [("_$i",i) for i in versions];
                         [("_$(string(i)[1])_$(string(i)[2])",i) for i in versions]]
    if Libdl.dlsym_e(dliculib, "u_strToUpper"*suf) != C_NULL
        @eval const version = $ver
        @eval const suffix  = $suf
        break
    end
end

_libicu(s, lib, p) = ( Symbol(string(p, s, suffix)), lib )

typealias UBool Int8
typealias UChar UInt16
typealias UErrorCode Int32

FAILURE(x::Integer) = x > 0
SUCCESS(x::Integer) = x <= 0
U_BUFFER_OVERFLOW_ERROR = 15

const locale   = String[""]

include("utext.jl")
include("ustring.jl")
include("ubrk.jl")
include("ucnv.jl")
include("ucol.jl")
include("ucsdet.jl")
include("udat.jl")
include("ucal.jl")
include("ucasemap.jl")

end # module ICU
