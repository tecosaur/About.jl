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
    name::Union{Symbol, Int}
    type::Type
end

function structinfo(T::Type)
    map(1:fieldcount(T)) do i
        if hassizeof(T)
            offset = fieldoffset(T, i) |> Int
            size = Int(if i < fieldcount(T)
                           fieldoffset(T, i+1)
                    else
                           sizeof(T)
                    end - fieldoffset(T, i))
            contentsize = if hassizeof(fieldtype(T, i))
                sizeof(fieldtype(T, i))
            else
                0
            end
            if contentsize > size # Pointer?
                contentsize = 0
            end
        else
            offset = size = contentsize = -1 # Cannot deduce easily
        end
        FieldInfo(i, FACE_CYCLE[mod1(i, length(FACE_CYCLE))],
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
            print(io, S"{shadow:(padded)} ")
        end
    elseif isabstracttype(type)
        print(io, "Abstract ")
    end
    if Base.issingletontype(type)
        print(io, "singleton ")
    end
    print(io, Base.summary(type))
    print(io, S" defined in {about_module:$(safeparentmodule(type))}, ")
    hassizeof(type) && print(io, "$(join(humansize(sizeof(type))))")
    print(io, "\n  ")
    supertypeinfo(io, type)
    (!isstructtype(type) || fieldcount(type) == 0) && return
    println(io, S"\n\nStruct with {bold:$(fieldcount(type))} fields:")
    fieldinfo = AnnotatedString[]
    if type isa DataType
        sinfo = structinfo(type)
        namepad = maximum(fi -> textwidth(string(fi.name)), sinfo) + 1
        for (; face, name, type, ispointer) in sinfo
            push!(fieldinfo, rpad(S"{$face:$name}", namepad) * S"{about_pointer:$(ifelse(ispointer, \"*\", \" \"))}$type")
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
        memorylayout(io, type)
    end
end

function safeparentmodule(type::Type)
    if type === Union{}
        Core
    else
        parentmodule(type)
    end
end

function supertypeinfo(io::IO, type::Type)
    if type === Union{}
        print(io, S"{julia_type:Union\{\}}")
        return
    end
    typestr(t) = highlight(sprint(show, Base.unwrap_unionall(t)))
    join(io, map(typestr, supertypes(type)),
         S" {julia_comparator:<:} ")
end

function memorylayout(io::IO, type::DataType)
    hassizeof(type) || return
    si = structinfo(type)
    !isempty(si) || return
    memstep = memstep = gcd((getfield.(si, :size), getfield.(si, :contentsize)) |>
        Iterators.flatten |> collect)
    memscale = max(1, floor(Int, 70/(sizeof(type)/memstep)))
    bars = AnnotatedString[]
    descs = AnnotatedString[]
    for (; i, size, contentsize, ispointer) in si
        size <= 0 && continue
        color = FACE_CYCLE[mod1(i, length(FACE_CYCLE))]
        width = max(2, memscale * size÷memstep)
        fsize, funits = humansize(size)
        desc = if ispointer
            cpad(S" {$color,bold:*} ", width)
        elseif contentsize < size
            csize, cunits = humansize(contentsize)
            psize, punits = humansize(size - contentsize)
            cpad(S" {$color:$csize$cunits}{shadow:+$psize$punits} ", width, ' ', RoundUp)
        else
            cpad(S" {$color:$fsize$funits} ", width)
        end
        push!(descs, desc)
        width = textwidth(desc)
        contentwidth = round(Int, width * contentsize / size)
        bar = S"{$color:$('■'^contentwidth)}"
        if contentsize < size
            paddwidth = width - contentwidth
            if ispointer
                bar *= S"{about_pointer,light:$('■'^paddwidth)}"
            else
                bar *= S"{shadow:$('■'^paddwidth)}"
            end
        end
        push!(bars, bar)
    end
    println(io)
    multirow_wrap(io, permutedims(hcat(bars, descs)))
    if any(i -> i.ispointer, si)
        println(io, S"\n {about_pointer,bold:*} = {about_pointer:Pointer} {light:(8B)}")
    end
end
