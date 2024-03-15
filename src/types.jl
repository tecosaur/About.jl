const POINTER_FACE = :cyan # should not appear in `FACE_CYCLE`

struct FieldInfo
    i::Int
    face::Union{Symbol, Face}
    offset::Int
    size::Int
    contentsize::Int
    ispointer::Bool
    name::Union{Symbol, Int}
    type::Type
end

function structinfo(T::Type)
    map(1:fieldcount(T)) do i
        if hassizeof(T) && hassizeof(fieldtype(T, i))
            offset = fieldoffset(T, i) |> Int
            size = Int(if i < fieldcount(T)
                           fieldoffset(T, i+1)
                    else
                           sizeof(T)
                    end - fieldoffset(T, i))
            contentsize = sizeof(fieldtype(T, i))
            if contentsize > size # Pointer?
                contentsize = 0
            end
        else
            offset = size = contentsize = -1 # Cannot deduce easily
        end
        FieldInfo(i, FACE_CYCLE[i % length(FACE_CYCLE) + 1],
                  offset,
                  size, contentsize,
                  contentsize == 0, # ispointer
                  fieldname(T, i), fieldtype(T, i))
    end
end

function about(io::IO, type::Type)
    if isprimitivetype(type)
        print(io, "Primitive ")
    elseif isconcretetype(type)
        print(io, "Concrete ")
        if Base.datatype_haspadding(type)
            print(io, styled"{shadow:(padded)} ")
        end
    elseif isabstracttype(type)
        print(io, "Abstract ")
    end
    if Base.issingletontype(type)
        print(io, "singleton ")
    end
    print(Base.summary(type))
    print(io, styled" defined in {bright_red:$(parentmodule(type))}, ")
    hassizeof(type) && print(io, "$(join(humansize(sizeof(type))))")
    println(io, "\n  ", supertypestr(type))
    (!isstructtype(type) || fieldcount(type) == 0) && return
    println(io, styled"\nStruct with {bold:$(fieldcount(type))} fields:")
    fieldinfo = AnnotatedString[]
    if type isa DataType
        sinfo = structinfo(type)
        namepad = maximum(fi -> textwidth(string(fi.name)), sinfo) + 1
        for (; face, name, type, ispointer) in sinfo
            push!(fieldinfo, rpad(styled"{$face:$name}", namepad) * styled"{$POINTER_FACE:$(ifelse(ispointer, \"*\", \" \"))}$type")
        end
    else
        for (; name, type) in structinfo(type)
            push!(fieldinfo, styled"$name{shadow:::$type}")
        end
    end
    if length(fieldinfo) < 32
        columnlist(io, fieldinfo, maxcols=1)
    else
        columnlist(io, fieldinfo, spacing=3)
    end
    if type isa DataType
        println(io)
        memorylayout(io, type)
    end
end

supertypestr(type::Type) =
    join(string.(supertypes(type)), styled" {red:<:} ")

function memorylayout(io::IO, type::DataType)
    si = structinfo(type)
    !isempty(si) || return
    memstep = memstep = gcd((getfield.(si, :size), getfield.(si, :contentsize)) |>
        Iterators.flatten |> collect)
    memscale = max(1, floor(Int, 70/(sizeof(type)/memstep)))
    bars = AnnotatedString[]
    descs = AnnotatedString[]
    for (; i, size, contentsize, ispointer) in si
        color = FACE_CYCLE[i % length(FACE_CYCLE) + 1]
        width = max(2, memscale * size÷memstep)
        color = FACE_CYCLE[i % length(FACE_CYCLE) + 1]
        fsize, funits = humansize(size)
        desc = if ispointer
            cpad(styled" {$color:*} ", width)
        elseif contentsize < size
            csize, cunits = humansize(contentsize)
            psize, punits = humansize(size - contentsize)
            cpad(styled" {$color:$csize$cunits}{shadow:+$psize$punits} ", width, ' ', RoundUp)
        else
            cpad(styled" {$color:$fsize$funits} ", width)
        end
        push!(descs, desc)
        width = textwidth(desc)
        contentwidth = round(Int, width * contentsize / size)
        bar = styled"{$color:$('■'^contentwidth)}"
        if contentsize < size
            color = if ispointer; :cyan else :light_black end
            paddwidth = width - contentwidth
            bar *= styled"{$color:$('■'^paddwidth)}"
        end
        push!(bars, bar)
    end
    multirow_wrap(io, permutedims(hcat(bars, descs)))
    if any(getfield.(si, :ispointer))
        print(io, styled"\n {$POINTER_FACE:*} = {$POINTER_FACE:Pointer} {light:(8B)}")
    end
    println(io)
end
