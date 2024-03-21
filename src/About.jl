module About

using Base: AnnotatedString, AnnotatedIOBuffer
using StyledStrings: @styled_str, Face, face!
using Pkg: dependencies
using InteractiveUtils

export about

include("utils.jl")
include("functions.jl")
include("types.jl")
include("values.jl")
include("modules.jl")

"""
    about(fn::Function, [signature::Tuple])
    about(typ::Type)
    about(mod::Module)
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

end
