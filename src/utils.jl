# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

const FACE_CYCLE = [:About_cycle1, :About_cycle2, :About_cycle3, :About_cycle4]

function humansize(bytes::Integer)
    units = ("B", "kB", "MB", "GB")
    magnitude = floor(Int, log(1024, 1 + bytes))
    if 10 < bytes < 10*1024^magnitude
        round(bytes / 1024^magnitude, digits=1)
    else
        round(Int, bytes / 1024^magnitude)
    end, units[1+magnitude]
end

"""
    hassizeof(type::Type)

Predict whether `sizeof(type)` is valid/will succeed.

The implementation is based on a reading of `jl_f_sizeof` in `builtins.c`.
"""
function hassizeof(@nospecialize(T::Type))
    if Base.isabstracttype(T) || T == Union{}
        false
    elseif T isa UnionAll || T isa Union
        Tu = Base.unwrap_unionall(T)
        # It would be good to call `jl_uniontype_size`
        Tu isa DataType
    elseif T ∈ (Symbol, String, Core.SimpleVector)
        false
    elseif @static if VERSION >= v"1.11" T <: GenericMemory else false end
        false
    elseif T isa DataType
        Base.isconcretetype(T)
    else
        Base.allocatedinline(T)
    end
end

function cpad(s, n::Integer, pad::Union{AbstractString, AbstractChar}=' ', r::RoundingMode = RoundToZero)
    rpad(lpad(s, div(n+textwidth(s), 2, r), pad), n, pad)
end

splural(n::Int) = ifelse(n == 1, "", "s")
splural(c::Vector) = splural(length(c))

function struncate(str::AbstractString, maxwidth::Int, joiner::Union{AbstractString, AbstractChar} = '…', mode::Symbol = :center)
    textwidth(str) <= maxwidth && return str
    left, right = firstindex(str), lastindex(str)
    width = textwidth(joiner)
    while true
        if mode ∈ (:right, :center)
            (width += textwidth(str[left])) <= maxwidth || break
            left = nextind(str, left)
        end
        if mode ∈ (:left, :center) && width < maxwidth
            (width += textwidth(str[right])) <= maxwidth || break
            right = prevind(str, right)
        end
    end
    str[begin:prevind(str, left)] * joiner * str[nextind(str, right):end]
end

function columnlist(io::IO, entries::Vector{<:AbstractString};
                    maxcols::Int=8, maxwidth::Int=last(displaysize(io)),
                    prefix::AbstractString = S"{emphasis:•} ", spacing::Int=2)
    isempty(entries) && return
    thecolumns = Vector{eltype(entries)}[]
    thecolwidths = Int[]
    for ncols in 1:maxcols
        columns = Vector{eltype(entries)}[]
        for col in Iterators.partition(entries, div(length(entries), ncols, RoundUp))
            push!(columns, collect(col))
        end
        widths = map.(textwidth, columns)
        colwidths = map(maximum, widths)
        layoutwidth = sum(colwidths) + ncols * textwidth(prefix) + (ncols - 1) * spacing
        if layoutwidth > maxwidth
            break
        else
            thecolumns, thecolwidths = columns, colwidths
        end
    end
    if isempty(thecolumns)
        thecolumns, thecolwidths = [entries], maximum(textwidth, entries)
    end
    for rnum in 1:length(first(thecolumns))
        for cnum in 1:length(thecolumns)
            rnum > length(thecolumns[cnum]) && continue
            cnum > 1 && print(io, ' '^spacing)
            print(io, prefix, rpad(thecolumns[cnum][rnum], thecolwidths[cnum]))
        end
        println(io)
    end
end

function multirow_wrap(io::IO, cells::Matrix{<:AbstractString};
                       indent::AbstractString = " ", maxwidth::Int=last(displaysize(io)))
    widths = map(textwidth, cells)
    colwidths = maximum(widths, dims=1)
    thiscol = textwidth(indent)
    segments = UnitRange{Int}[1:0]
    for (i, (col, width)) in enumerate(zip(eachcol(cells), colwidths))
        if thiscol + width > maxwidth
            push!(segments, last(last(segments))+1:i-1)
            thiscol = textwidth(indent) + width
        else
            thiscol += width
        end
    end
    push!(segments, last(last(segments))+1:size(cells, 2))
    filter!(!isempty, segments)
    for segment in segments
        for row in eachrow(cells[:, segment])
            println(io, indent, join(row))
        end
    end
end

"""
    wraplines(content::AnnotatedString, width::Integer = 80, column::Integer = 0)

Wrap `content` into a vector of lines of at most `width` (according to
`textwidth`), with the first line starting at `column`.
"""
function wraplines(content::Union{Annot, SubString{<:Annot}}, width::Integer = 80, column::Integer = 0) where { Annot <: AnnotatedString}
    s, lines = String(content), SubString{Annot}[]
    i, lastwrap, slen = firstindex(s), 0, ncodeunits(s)
    most_recent_break_opportunity = 1
    while i < slen
        if isspace(s[i]) && s[i] != '\n'
            most_recent_break_opportunity = i
        elseif s[i] == '\n'
            push!(lines, content[nextind(s, lastwrap):prevind(s, i)])
            lastwrap = i
            column = 0
        elseif column >= width && most_recent_break_opportunity > 1
            if lastwrap == most_recent_break_opportunity
                nextbreak = findfirst(isspace, @view s[nextind(s, lastwrap):end])
                if isnothing(nextbreak)
                    break
                else
                    most_recent_break_opportunity = lastwrap + nextbreak
                end
                i = most_recent_break_opportunity
            else
                i = nextind(s, most_recent_break_opportunity)
            end
            push!(lines, content[nextind(s, lastwrap):prevind(s, most_recent_break_opportunity)])
            lastwrap = most_recent_break_opportunity
            column = 0
        end
        column += textwidth(s[i])
        i = nextind(s, i)
    end
    if lastwrap < slen
        push!(lines, content[nextind(s, lastwrap):end])
    end
    lines
end
