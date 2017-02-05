using BinDeps

include("../src/compat5.jl")

include("versions.jl")

@BinDeps.setup

icu_aliases = ["libicuuc"]
i18n_aliases = ["libicui18n"]
@static if is_apple()
    icu_aliases = ["libicucore"]
    i18n_aliases = ["libicucore"]
end
@static if is_windows()
    icu_aliases = ["icuuc$v" for v in versions]
    i18n_aliases = [["icui18n$v" for v in versions];
                    ["icuin$v" for v in versions]]
end
icu = library_dependency("icu", aliases=icu_aliases)
icui18n = library_dependency("icui18n", aliases=i18n_aliases)

@static if is_windows()
    using WinRPM
    provides(WinRPM.RPM, "icu", [icu,icui18n], os=:Windows)
end
dict = Dict{String, Any}()
for v in apt_versions
    push!(dict, "libicu$v" => [icu, icui18n])
end
provides(AptGet, dict)
#provides(AptGet, Dict("libicu$v" => [icu,icui18n] for v in apt_versions))
provides(Yum, "icu", [icu, icui18n])

@BinDeps.install Dict(:icu => :iculib, :icui18n => :iculibi18n)
