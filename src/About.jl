module About

using StyledStrings: @styled_str, Face, face!, addface!
using JuliaSyntaxHighlighting: highlight
using InteractiveUtils

@static if VERSION >=v"1.11-alpha"
    using Base: AnnotatedString, AnnotatedIOBuffer
else
    using StyledStrings: AnnotatedString, AnnotatedIOBuffer
end

using PrecompileTools: @compile_workload

const var"@S_str" = var"@styled_str"

export about

include("utils.jl")
include("functions.jl")
include("types.jl")
include("values.jl")

"""
    about([io::IO], fn::Function, [argtypes::Type...])
    about([io::IO], typ::Type)
    about([io::IO], val::Any)

Display information on the particular nature of the argument, whatever it may be.

TODO mention the mechanisms for extension here too.
"""
function about end

about(x) = about(stderr, x)
function about(xs...)
    if first(xs) == stderr
        throw(MethodError(about, xs))
    else
        about(stderr, xs...)
    end
end

"""
    memorylayout(io::IO, T::DataType)
    memorylayout(io::IO, val::T)

Print to `io` the memory layout of the type `T`, or `val` a particular instance
of the type.

Specialised implementations should be implemented freely to enhance the utility
and prettiness of the display.
"""
function memorylayout end

"""
    elaboration(::IO, x::Any)

Elaborate on `x` to io, providing extra information that might be of interest
seperately from `about` or `memorylayout`.

Specialised implementations should be implemented freely to enhance the utility
and prettiness of the display.

By convention, this is not invoked when displaying `x` compactly.
"""
elaboration(::IO, ::Any) = nothing

const ABOUT_FACES = [
    :about_module => Face(foreground=:bright_red),
    :about_pointer => Face(foreground=:cyan),
    :about_count => Face(weight=:bold),
    :about_bytes => Face(weight=:bold),
]

__init__() = foreach(addface!, ABOUT_FACES)

@compile_workload begin
    about(devnull, FieldInfo)
    about(devnull, 1)
    about(devnull, 1.0)
    about(devnull, nothing)
end

end
