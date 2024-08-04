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
    about([io::IO], thing)

Display information (to `io` if provided) on the particular nature of `thing`,
whatever it may be.

This is implemented for a variety of types and objects, with general
implementations for:
- Functions
- Types
- Values
- Modules

Not sure what to make of this? Just try it out 😉

!!! tip "Tip for package developers"
    See the *extended help* for information on how to implement specialised forms for
    your own objects. Also consider putting specialised display methods in a package
    extension.

# Extended help

While it is possible to extend `about` by implementing specialised `about(::IO,
::MyThing)` methods, it is better to specialise the two main functions called by
the `about` function for values (not a `Function`, `Type`, or `Module`):
- `memorylayout(::IO, ::MyThing)`
- `elaboration(::IO, ::MyThing)`

Either or both of these functions can be specialised to customise the display
from `about(::IO, ::MyThing)`, and are involved in the output like so:

```julia-repl
julia> about(mything)

[name] [type hierachy] [size]

[memorylayout]

[elaboration (when non-compact)]
```

As can be guessed from the names, it is expected that `memorylayout` prints an
informative representation of the memory layout of its argument. See
`src/values.jl` within the `About` package source for some examples (just scroll
past the initial generic implementation).

Unlike `memorylayout`, the `elaboration` function does not print anything by
default, but can be specialised to add extra pieces of useful information that
don't relate to the in-memory representation of the object itself.
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
