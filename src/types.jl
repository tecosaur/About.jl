# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

struct FieldInfo
    i::Int
    face::Union{Symbol, Face}
    offset::Int
    size::Int
    contentsize::Int
    ispointer::Bool
    tagsize::Int
    name::Union{Symbol, Int}
    type::Type
end

struct AlignmentRevealer{T}
    t::T
    _::UInt8
end

function structinfo(@nospecialize(T::Type))
    map(1:fieldcount(T)) do i
        ispointer = false
        tagsize = 0
        if hassizeof(T)
            offset = fieldoffset(T, i) |> Int
            size = Int(if i < fieldcount(T)
                           fieldoffset(T, i+1)
                       else
                           sizeof(T)
                       end - fieldoffset(T, i))
            Tf = fieldtype(T, i)
            if Tf isa Union && hassizeof(Tf)
                contentsize = sizeof(Tf)
                if contentsize < fieldoffset(AlignmentRevealer{Tf}, 2)
                    tagsize = fieldoffset(AlignmentRevealer{Tf}, 2) - contentsize
                else
                    ispointer = true
                end
            else
                if hassizeof(Tf)
                    contentsize = sizeof(Tf)
                    if contentsize > size
                        contentsize = sizeof(Ptr{Nothing})
                        ispointer = true
                    end
                else
                    contentsize = sizeof(Ptr{Nothing})
                    ispointer = true
                end
            end
        else
            offset = size = contentsize = -1 # Cannot deduce easily
        end
        FieldInfo(i, FACE_CYCLE[mod1(i, length(FACE_CYCLE))],
                  offset,
                  size, contentsize,
                  ispointer, tagsize,
                  fieldname(T, i), fieldtype(T, i))
    end
end

function about(io::IO, type::Type)
    if isprimitivetype(type)
        print(io, "Primitive ")
    elseif isconcretetype(type)
        print(io, "Concrete ")
        if Base.datatype_haspadding(type)
            print(io, S"{shadow:(padded)} ")
        end
    elseif isabstracttype(type)
        print(io, "Abstract ")
    end
    if Base.issingletontype(type)
        print(io, "singleton ")
    end
    print(io, Base.summary(type))
    print(io, S" defined in {About_module:$(safeparentmodule(type))}, ")
    hassizeof(type) && print(io, "$(join(humansize(sizeof(type))))")
    print(io, "\n  ")
    supertypeinfo(io, type)
    if (!isstructtype(type) || type isa UnionAll || fieldcount(type) == 0)
        println(io)
        return
    end
    print(io, S"\n\nStruct with {bold:$(fieldcount(type))} fields:")
    fieldinfo = AnnotatedString[]
    if type isa DataType
        sinfo = structinfo(type)
        namepad = maximum(fi -> textwidth(string(fi.name)), sinfo) + 1
        for (; face, name, type, ispointer) in sinfo
            push!(fieldinfo, rpad(S"{$face:$name}", namepad) * S"{About_pointer:$(ifelse(ispointer, \"*\", \" \"))}$type")
        end
    else
        for (; name, type) in structinfo(type)
            push!(fieldinfo, S"$name{shadow:::$type}")
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
    println(io)
end

function safeparentmodule(type::Type)
    if type === Union{}
        Core
    elseif type isa Union
        utypes = [safeparentmodule(t) for t in Base.uniontypes(type)]
        if allequal(utypes)
            first(utypes)
        else
            Core
        end
    else
        parentmodule(type)
    end
end

function supertypeinfo(io::IO, type::Type)
    typestr(t) = highlight(sprint(show, Base.unwrap_unionall(t)))
    ptype = if type === Union{}
        print(io, S"{julia_type:Union\{\}}")
        return
    elseif type isa Union
        print(io, typestr(type), S" {julia_comparator:<:} ")
        foldl(typejoin, Base.uniontypes(type))
    else
        type
    end
    join(io, map(typestr, supertypes(ptype)),
         S" {julia_comparator:<:} ")
end

function memorylayout(io::IO, type::DataType)
    hassizeof(type) || return
    si = structinfo(type)
    (isempty(si) || iszero(sizeof(type))) && return
    memstep = memstep = gcd((getfield.(si, :size), getfield.(si, :contentsize)) |>
        Iterators.flatten |> collect)
    memscale = max(1, floor(Int, 70 / (sizeof(type) / memstep)))
    bars = AnnotatedString[]
    descs = AnnotatedString[]
    for (; i, size, contentsize, tagsize, ispointer) in si
        size <= 0 && continue
        color = FACE_CYCLE[mod1(i, length(FACE_CYCLE))]
        width = max(2, memscale * size÷memstep)
        fsize, funits = humansize(size)
        desc = if ispointer
            cpad(S" {$color,bold:*} ", width)
        elseif contentsize < size
            csize, cunits = humansize(contentsize)
            psize, punits = humansize(size - contentsize - tagsize)
            if psize == 0 # entirely tag bytes
                cpad(S" {$color:$csize$cunits}{About_tag:+⚑$(tagsize)B} ", width, ' ', RoundUp)
            elseif tagsize > 0 # tag and padding bytes
                cpad(S" {$color:$csize$cunits}{About_tag:+⚑$(tagsize)B}{shadow:+$psize$punits} ", width, ' ', RoundUp)
            else # only padding bytes
                cpad(S" {$color:$csize$cunits}{shadow:+$psize$punits} ", width, ' ', RoundUp)
            end
        else
            cpad(S" {$color:$fsize$funits} ", width)
        end
        push!(descs, desc)
        width = textwidth(desc)
        contentwidth = round(Int, width * contentsize / size)
        bar = S"{$color:$('■'^contentwidth)}"
        if contentsize < size
            tagwidth = round(Int, width * tagsize / size)
            paddwidth = width - contentwidth - tagwidth
            if tagwidth > 0 # ⚑ ⮼ ▬
                bar *= S"{About_tag:$('■'^tagwidth)}"
            end
            if ispointer
                bar *= S"{About_pointer,light:$('■'^paddwidth)}"
            else
                bar *= S"{shadow:$('■'^paddwidth)}"
            end
        end
        push!(bars, bar)
    end
    multirow_wrap(io, permutedims(hcat(bars, descs)))
    if any(i -> i.ispointer || i.tagsize > 0, si)
        println(io)
        any(i -> i.ispointer, si) &&
            print(io, S"\n {About_pointer,bold:*} = {About_pointer:Pointer} {light:(8B)}")
        any(i -> i.tagsize > 0, si) &&
            print(io, S"\n {About_tag:⚑} = Tag bytes")
    end
    nothing
end
