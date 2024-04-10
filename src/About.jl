module About

using Base: AnnotatedString, AnnotatedIOBuffer
using StyledStrings: @styled_str, Face, face!, addface!
using JuliaSyntaxHighlighting: highlight
using InteractiveUtils

export about

include("utils.jl")
include("functions.jl")
include("types.jl")
include("values.jl")

"""
    about(fn::Function, [signature::Tuple])
    about(typ::Type)
    about(obj::Any)

Display information on the particular nature of the argument, whether
it be a function, type, or value.
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

const ABOUT_FACES = [
    :about_module => Face(foreground=:bright_red),
    :about_pointer => Face(foreground=:cyan),
    :about_count => Face(weight=:bold),
    :about_bytes => Face(weight=:bold),
]

__init__() = foreach(addface!, ABOUT_FACES)

end
