# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

module PkgExt

using Pkg
using StyledStrings
using About: About, about, columnlist

using PrecompileTools: @compile_workload

function About.about_pkg(io::IO, pkg::Base.PkgId, mod::Module)
    isnothing(pkgversion(mod)) ||
        print(io, styled"  Version {about_module:$(pkgversion(mod))}")
    srcdir = pkgdir(mod)
    if isnothing(srcdir)
        print(io, styled" (builtin)")
    else
        srcdir = Base.fixup_stdlib_path(srcdir)
        srcdir = something(Base.find_source_file(srcdir), srcdir)
        srcdir = contractuser(srcdir)
        print(io, styled" loaded from {light,underline:$srcdir}")
    end
    println(io)
    isnothing(srcdir) && return
    manifest_file = Pkg.Types.manifestfile_path(pkgdir(mod))
    project_file = Pkg.Types.projectfile_path(pkgdir(mod))
    thedeps = if !isnothing(manifest_file) && isfile(manifest_file)
        Pkg.Types.read_manifest(manifest_file).deps
    else
        Pkg.dependencies()
    end
    directdeps = if haskey(thedeps, pkg.uuid)
        listdeps(thedeps, pkg.uuid)
    elseif !isnothing(project_file) && isfile(project_file)
        collect(values(Pkg.Types.read_project(project_file).deps))
    else
        collect(keys(thedeps))
    end
    isempty(directdeps) && return
    depstrs = map(directdeps) do dep
        indirectextras = length(alldeps(thedeps, dep, directdeps))
        if indirectextras > 0
            styled"$(thedeps[dep].name) {shadow:(+$indirectextras)}"
        else
            styled"$(thedeps[dep].name)"
        end
    end
    indirect_depcount = length(alldeps(thedeps, pkg.uuid) ∪ directdeps) - length(depstrs)
    indirect_info = if indirect_depcount > 0
        styled" {shadow:(+$indirect_depcount indirectly)}"
    else styled"" end
    println(io, styled"\n{bold:Directly depends on {emphasis:$(length(directdeps))} \
                       package$(ifelse(length(directdeps) == 1, \"\", \"s\"))}$indirect_info:")
    columnlist(io, depstrs)
end

function listdeps(deps::Dict{Base.UUID, Pkg.Types.PackageEntry}, pkg::Base.UUID)
    if haskey(deps, pkg)
        collect(values(deps[pkg].deps))
    else
        Base.UUID[]
    end
end

function listdeps(deps::Dict{Base.UUID, Pkg.API.PackageInfo}, pkg::Base.UUID)
    if haskey(deps, pkg)
        collect(values(deps[pkg].dependencies))
    else
        Base.UUID[]
    end
end

function alldeps(deps::Dict{Base.UUID, <:Union{Pkg.Types.PackageEntry, Pkg.API.PackageInfo}}, pkg::Base.UUID, ignore::Vector{Base.UUID} = Base.UUID[])
    depcheck = listdeps(deps, pkg)
    depcollection = Set{Base.UUID}()
    while !isempty(depcheck)
        id = popfirst!(depcheck)
        id in depcollection || id in ignore && continue
        append!(depcheck, listdeps(deps, id))
        push!(depcollection, id)
    end
    collect(depcollection)
end

@compile_workload begin
    about(devnull, @__MODULE__)
end

end
