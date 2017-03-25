if VERSION < v"0.5.0"
    # JuliaLang/julia#16219
    if !isdefined(Base, Symbol("@static"))
        macro static(ex)
            if isa(ex, Expr)
                if ex.head === :if
                    cond = eval(current_module(), ex.args[1])
                    if cond
                        return esc(ex.args[2])
                    elseif length(ex.args) == 3
                        return esc(ex.args[3])
                    else
                        return nothing
                    end
                end
            end
            throw(ArgumentError("invalid @static macro"))
        end
    end
    @eval is_windows() = $(OS_NAME == :Windows)
    @eval is_apple()   = $(OS_NAME == :Darwin)
    @eval is_linux()   = $(OS_NAME == :Linux)
    @eval is_bsd()     = $(OS_NAME in (:FreeBSD, :OpenBSD, :NetBSD, :Darwin, :Apple))
    @eval is_unix()    = $(is_linux() || is_bsd())

    const ASCIIStr  = ASCIIString
    const UTF8Str   = UTF8String
    const UniStr    = UTF16String
    const ByteStr   = ByteString

    const cvt_utf8  = utf8
    const cvt_utf16 = utf16
else
    using LegacyStrings
    const ASCIIStr  = LegacyStrings.ASCIIString
    const UTF8Str   = LegacyStrings.UTF8String
    const UniStr    = LegacyStrings.UTF16String
    const ByteStr   = Union{ASCIIStr, UTF8Str, String}

    const cvt_utf8  = LegacyStrings.utf8
    const cvt_utf16 = LegacyStrings.utf16
end
