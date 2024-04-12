const FACE_CYCLE = [:bright_blue, :bright_green, :bright_yellow, :bright_magenta]

function humansize(bytes::Integer)
    units = ("B", "kB", "MB", "GB")
    magnitude = floor(Int, log(1024, 1 + bytes))
    if 10 < bytes < 10*1024^magnitude
        round(bytes / 1024^magnitude, digits=1)
    else
        round(Int, bytes / 1024^magnitude)
    end, units[1+magnitude]
end

function hassizeof(type::Type)
    !isconcretetype(type) && return false
    type <: GenericMemory && return false
    type in (Symbol, String, Core.SimpleVector) && return false
    true
end

function cpad(s, n::Integer, pad::Union{AbstractString, AbstractChar}=' ', r::RoundingMode = RoundToZero)
    rpad(lpad(s, div(n+textwidth(s), 2, r), pad), n, pad)
end

function struncate(str::AbstractString, maxwidth::Int, joiner::AbstractString = "…", mode::Symbol = :center)
    textwidth(str) <= maxwidth && return str
    left, right = firstindex(str) - 1, lastindex(str) + 1
    width = textwidth(joiner)
    while width < maxwidth
        if mode ∈ (:right, :center)
            left = nextind(str, left)
            width += textwidth(str[left])
        end
        if mode ∈ (:left, :center) && width < maxwidth
            right = prevind(str, right)
            width += textwidth(str[right])
        end
    end
    str[begin:left] * joiner * str[right:end]
end

function columnlist(io::IO, entries::Vector{<:AbstractString};
                    maxcols::Int=8, maxwidth::Int=last(displaysize(io)),
                    prefix::AbstractString = styled"{emphasis:•} ", spacing::Int=2)
    thecolumns = Vector{eltype(entries)}[]
    thecolwidths = Int[]
    for ncols in 1:maxcols
        columns = Vector{eltype(entries)}[]
        for col in Iterators.partition(entries, length(entries) ÷ ncols)
            push!(columns, collect(col))
        end
        widths = map.(textwidth, columns)
        colwidths = map(maximum, widths)
        if sum(colwidths) + ncols * textwidth(prefix) + (1 - ncols) * spacing > maxwidth
            break
        else
            thecolumns, thecolwidths = columns, colwidths
        end
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
