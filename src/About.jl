# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

"""
    About

Sometimes you want to know more *about* what you're working with, whether it be a
function, type, value, or something else entirely.

This package is a utility to help answer that question, it exports a single
function `about`, which can be applied to any Julia object.

# Extended Help

## Examples

**Applied to a Module**

```julia-repl
julia> about(About)
Module About [69d22d85-9f48-4c46-bbbe-7ad8341ff72a]
  Version 0.1.0 loaded from ~/.julia/dev/About

Directly depends on 4 packages (+9 indirectly):
• PrecompileTools (+5)  • InteractiveUtils (+3)  • StyledStrings  • JuliaSyntaxHighlighting (+1)

Exports 2 names:
• About  • about
```

**Applied to a singleton value**

```julia-repl
julia> about(ℯ)
Irrational{:ℯ} (<: AbstractIrrational <: Real <: Number <: Any), occupies 0B.
singleton
```

**Applied to a float**

```julia-repl
julia> about(Float64(ℯ))
Float64 (<: AbstractFloat <: Real <: Number <: Any), occupies 8B.

 0100000000000101101111110000101010001011000101000101011101101001
 ╨└────┬────┘└────────────────────────┬─────────────────────────┘
 +    2^1   ×                1.359140914229522545
 = 2.7182818284590451
```

**Applied to a character**

```julia-repl
julia> about('√')
Char (<: AbstractChar <: Any), occupies 4B.

     ┌2─┐   ┌2─┐┌──1──┐┌A─┐
 11100010 10001000 10011010 00000000
 └─0xe2─┘ └─0x88─┘ └─0x9a─┘ └─0x00─┘
 = U+221A

 Unicode '√', category: Symbol, math (Sm)
```

**Applied to a struct type**

```julia-repl
julia> about(Dict{Symbol, Int})
Concrete DataType defined in Base, 64B
  Dict{Symbol, Int64} <: AbstractDict{Symbol, Int64} <: Any

Struct with 8 fields:
• slots    *Memory{UInt8}
• keys     *Memory{Symbol}
• vals     *Memory{Int64}
• ndel      Int64
• count     Int64
• age       UInt64
• idxfloor  Int64
• maxprobe  Int64

 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
    *       *       *       8B      8B      8B      8B      8B

 * = Pointer (8B)
```

**Applied to a function**

```julia-repl
julia> about(sum, Set{Int})
sum (generic function with 10 methods)
 Defined in Base(8) extended in Base.MPFR(1) and Base.GMP(1).

 Matched 1 method :: Int64
  sum(a; kw...) @ Base reduce.jl:561

 Method effects
  ✗ consistent     might not return or terminate consistently
  ✔ effect free    guaranteed to be free from externally semantically visible side effects
  ✗ no throw       may throw an exception
  ✗ terminates     might not always terminate
  ✔ no task state  guaranteed not to access task state (allowing migration between tasks)
  ~ inaccessible memory only  may access or modify mutable memory iff pointed to by its call arguments
  ✗ no undefined behaviour  may execute undefined behaviour
  ✔ non-overlayed  may call methods from an overlayed method table
```

## Faces

These are the faces that `About` defines, and can be customised to change the
style of the output.

- `about_module` (bright red)
- `about_pointer` (cyan)
- `about_count` (bold)
- `about_bytes` (bold)
- `about_cycle1` (bright blue)
- `about_cycle2` (bright green)
- `about_cycle3` (bright yellow)
- `about_cycle4` (bright magenta)

## Public API

- `about`
- `memorylayout` (extensible)
- `elaboration` (extensible)
"""
module About

using StyledStrings: @styled_str, Face, face!, addface!
using JuliaSyntaxHighlighting
using InteractiveUtils

@static if v"1.11-alpha" <= VERSION < v"1.12-alpha"
    using Base: AnnotatedString, AnnotatedIOBuffer
    const highlight = identity
else
    using StyledStrings: AnnotatedString, AnnotatedIOBuffer
    const highlight = JuliaSyntaxHighlighting.highlight
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
    :about_cycle1 => Face(inherit=:bright_blue),
    :about_cycle2 => Face(inherit=:bright_green),
    :about_cycle3 => Face(inherit=:bright_yellow),
    :about_cycle4 => Face(inherit=:bright_magenta),
]

__init__() = foreach(addface!, ABOUT_FACES)

@compile_workload begin
    about(devnull, FieldInfo)
    about(devnull, 1)
    about(devnull, 1.0)
    about(devnull, nothing)
end

end
