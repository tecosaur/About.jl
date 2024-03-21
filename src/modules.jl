function about(io::IO, mod::Module)
    print(io, Base.summary(mod), " ", styled"{bright_red:$mod}")
    pkg = Base.identify_package(string(mod))
    # if is a package
    if !isnothing(pkg)
        println(io, styled" {shadow:[$(pkg.uuid)]}")
        deps = dependencies()
        idx = findfirst(pkginfo -> pkginfo.name == string(mod), collect(values(deps)))
        pkginfo = collect(values(deps))[idx]
        if !isnothing(pkgversion(mod))
            if isnothing(pkginfo.version)
                print(io, styled"  {bright_red:Standard library}")
            else
                print(io, styled"  Version {bright_red:$(pkgversion(mod))}")
            end
        end
        if !isnothing(pkgdir(mod))
            println(io, styled" loaded from {bright_blue:$(pkgdir(mod))}\n")
        end
        print_dependencies(io, mod)
    else
        println(io)
    end
    println(io)
    print_names(io, mod)
end

function print_dependencies(io::IO, mod::Module)
    deps = dependencies()
    idx = findfirst(pkg -> pkg.name == string(mod), collect(values(deps)))
    isnothing(idx) && return
    pkg = collect(values(deps))[idx]
    println(io, styled"Depends directly on {bright_blue:$(length(pkg.dependencies))} packages:")
    pkgdeps = sort(map(string, collect(keys(pkg.dependencies))))
    columnlist(io, pkgdeps)
end

function print_names(io::IO, mod::Module)
    println(io, styled"Exports {bright_blue:$(length(names(mod)))} names:")
    pkgnames = map(string, names(mod))
    columnlist(io, pkgnames)
end
