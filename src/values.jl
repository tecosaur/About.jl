# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# ------------------
# Structs, in general
# ------------------

function about(io::IO, value::T) where {T}
    # Type information
    iotype = AnnotatedIOBuffer()
    print(iotype, Base.summary(value))
    ismutable(value) && print(iotype, " (mutable)")
    print(iotype, S" ({julia_comparator:<:} ")
    supertypeinfo(iotype, supertype(T))
    print(iotype, ")")
    infotype = read(seekstart(iotype), AnnotatedString)
    # Size information
    typesize = try sizeof(T) catch _ sizeof(value) end
    datasize = sizeof(value)
    netsize = Base.summarysize(value)
    infosize = if typesize == datasize == netsize
        S"{about_bytes:$(join(humansize(typesize)))}."
    elseif typesize == datasize <= netsize
        S"{about_bytes:$(join(humansize(typesize)))} directly \
          (referencing {about_bytes:$(join(humansize(netsize)))} in total)"
    elseif typesize == datasize > netsize
        S"{about_bytes:$(join(humansize(typesize)))} directly \
          ({warning:!} referencing {about_bytes:$(join(humansize(netsize)))} in total, \
          {warning:strangely less than the direct, \
          {underline,link={https://github.com/tecosaur/About.jl}:\
          please open an issue on About.jl with this example}})"
    else # all different
        S"{about_bytes:$(join(humansize(typesize)))} directly \
          (referencing {about_bytes:$(join(humansize(netsize)))} in total, \
          holding {about_bytes:$(join(humansize(datasize)))} of data)"
    end
    print(io, infotype)
    if textwidth(infotype) < last(displaysize(io)) &&
        textwidth(infotype) + textwidth(infosize) + 12 >= last(displaysize(io))
        print(io, "\n Memory footprint: ")
    else
        print(io, ", occupies ")
    end
    println(io, infosize)
    # Layout + elaboration
    memorylayout(io, value)
    if get(io, :compact, false) != true
        elaboration(io, value)
    end
end

function memorylayout(io::IO, value::T) where {T}
    if isprimitivetype(T)
        get(io, :compact, false) || print(io, "\n ")
        print(io, bitstring(value))
        return
    end
    if get(io, :compact, false) == true
        print(io, "«struct»")
        return
    end
    if Base.issingletontype(T)
        println(io, S"{italic:singleton}")
        return
    end
    sinfo = structinfo(T)
    isempty(sinfo) && return
    ffaces = Union{Face, Symbol}[]
    fnames = String[]
    ftypes = String[]
    fsizes = String[]
    freprs = AnnotatedString[]
    fshows = AnnotatedString[]
    for (; face, name, type, size, ispointer) in sinfo
        size <= 0 && continue
        push!(ffaces, face)
        push!(fnames, string(name))
        push!(ftypes, string(type))
        push!(fsizes, join(humansize(size)))
        aio = AnnotatedIOBuffer()
        if !isdefined(value, name)
            push!(freprs, S"{julia_builtin:#undef}")
            push!(fshows, S"Uninitialised value")
            continue
        end
        fvalue = getfield(value, name)
        if Base.issingletontype(typeof(fvalue))
            push!(freprs, S"{shadow:singleton}")
        elseif size == 0
            push!(freprs, S"{error:??}")
        elseif ispointer
            try
                pt = pointer(fvalue)
                push!(freprs, S"{about_pointer:@ $(sprint(show, UInt64(pt)))}")
            catch
                push!(freprs, S"{about_pointer:Ptr?}")
            end
        else
            memorylayout(IOContext(aio, :compact => true), fvalue)
            push!(freprs, read(seekstart(aio), AnnotatedString))
        end
        truncate(aio, 0)
        show(IOContext(aio, :compact => true), fvalue)
        push!(fshows, read(seekstart(aio), AnnotatedString))
    end
    width = last(displaysize(io)) - 2
    namewidth = maximum(textwidth, fnames, init=0)
    typewidth = min(maximum(textwidth, ftypes, init=0), width ÷ 4)
    sizewidth = maximum(textwidth, fsizes, init=0)
    width -= 1 + namewidth + 1 + typewidth + 2 + sizewidth
    reprwidth = min((2 * width) ÷ 3, maximum(textwidth, freprs, init=0))
    showwidth = width - reprwidth
    for (face, name, type, size, brepr, shown) in zip(ffaces, fnames, ftypes, fsizes, freprs, fshows)
        println(io, ' ',
                S"{$face:$(lpad(name, namewidth)){shadow:::}$(rpad(struncate(type, typewidth, \"…\", :right), typewidth)) $(lpad(size, sizewidth))}",
                ' ', rpad(struncate(brepr, reprwidth, S" {shadow:…} "), reprwidth),
                ' ', face!(struncate(shown, showwidth, S" {shadow:…} "), face))
    end
    memorylayout(io, T)
end

# ------------------
# Modules
# ------------------

function about(io::IO, mod::Module)
    pkg = nothing
    for (bpkg, m) in Base.loaded_modules
        if m == mod
            pkg = bpkg
            break
        end
    end
    !isnothing(pkg) && !applicable(about_pkg, io, pkg, mod) &&
        Base.require(Base.PkgId(Base.UUID("44cfe95a-1eb2-52ea-b672-e2afdf69b78f"), "Pkg"))
    print(io, S"{bold:Module {about_module:$mod}}")
    if !isnothing(pkg)
        println(io, S" {shadow:[$(something(pkg.uuid, \"no uuid\"))]}")
        Base.invokelatest(about_pkg, io, pkg, mod)
    else
        println(io)
    end
    function classify(m::Module, name::Symbol)
        isdefined(m, name) ||
            return (; name, str = S"$name {shadow:(undefined)}", kind=:undefined, parent=m, order=5)
        val = getglobal(m, name)
        order, kind, face, parent = if val isa Module
            0, :module, :about_module, val
        elseif val isa Function && first(String(name)) == '@'
            1, :macro, :julia_macro, parentmodule(val)
        elseif val isa Function
            2, :function, :julia_funcall, parentmodule(val)
        elseif val isa Type
            3, :type, :julia_type, if val isa UnionAll || val isa Union
                m else parentmodule(val) end
        else
            4, :value, :julia_identifier, if Base.issingletontype(typeof(val))
                parentmodule(typeof(val))
            else
                m
            end
        end
        while parentmodule(parent) ∉ (parent, Main)
            parent = parentmodule(parent)
        end
        (; name, str = S"{code,$face:$name}", kind, parent, order)
    end
    classify(m::Module, names::Vector{Symbol}) =
        sort(map(Base.Fix1(classify, m), names), by=x->x.order)
    allnames = classify(mod, names(mod))
    exports = similar(allnames, 0)
    reexports = similar(allnames, 0)
    publics = similar(allnames, 0)
    for exp in allnames
        if exp.parent === mod && Base.isexported(mod, exp.name)
            push!(exports, exp)
        elseif exp.parent === mod && Base.ispublic(mod, exp.name)
            push!(publics, exp)
        elseif exp.parent !== mod
            push!(reexports, exp)
        end
    end
    if !isempty(exports)
        println(io, S"\n{bold:Exports {emphasis:$(length(exports))} name$(splural(exports)):}")
        columnlist(io, map(x->x.str, exports))
    end
    if !isempty(reexports)
        parents = join(sort(map(p->S"{about_module:$p}", unique(map(x->x.parent, reexports)))), ", ")
        println(io, S"\n{bold:Re-exports {emphasis:$(length(reexports))} name$(splural(reexports))} (from $parents){bold::}")
        columnlist(io, map(x->x.str, reexports))
    end
    if !isempty(publics)
        println(io, S"\n{bold:Public API ({emphasis:$(length(publics))} name$(splural(publics))):}")
        columnlist(io, map(x->x.str, publics))
    end
end

function about_pkg end # Implemented in `../ext/PkgExt.jl`

# ------------------
# Numeric types
# ------------------

const NUMBER_BIT_FACES = (
    sign = :bright_blue,
    exponent = :bright_green,
    mantissa = :bright_red
)

function memorylayout(io::IO, value::Bool)
    bits = AnnotatedString(bitstring(value))
    face!(bits[1:end-1], :shadow)
    face!(bits[end:end], NUMBER_BIT_FACES.sign)
    if get(io, :compact, false) == true
        print(io, bits)
    else
        println(io, "\n ", bits, S" {bold:=} $value")
    end
end

function memorylayout(io::IO, value::Union{UInt8, UInt16, UInt32, UInt64, UInt128})
    bits = AnnotatedString(bitstring(value))
    for (; match) in eachmatch(r"0+", bits)
        face!(match, :shadow)
    end
    if get(io, :compact, false) == true
        print(io, bits)
    else
        println(io, "\n ", bits, ifelse(sizeof(value) > 4, "\n", ""),
                S" {bold:=} $value")
    end
end

function memorylayout(io::IO, value::Union{Int8, Int16, Int32, Int64, Int128})
    bits = AnnotatedString(bitstring(value))
    face!(bits[1:1], NUMBER_BIT_FACES.sign)
    for (; match) in eachmatch(r"0+", bits)
        if match.offset == 0
            match = bits[2:match.ncodeunits]
        end
        face!(match, :shadow)
    end
    if get(io, :compact, false) == true
        print(io, bits)
    else
        signstr = ifelse(value < 0, '-', '+')
        println(io, "\n ", bits, ifelse(sizeof(value) > 4, "\n", ""),
                S" {bold:=} {$(NUMBER_BIT_FACES.sign):$signstr}$(abs(widen(value)))")
    end
end

memorylayout(io::IO, float::Float64) = floatlayout(io, float, 11)
memorylayout(io::IO, float::Float32) = floatlayout(io, float, 8)
memorylayout(io::IO, float::Float16) = floatlayout(io, float, 5)
@static if VERSION >=v"1.11-alpha"
    memorylayout(io::IO, float::Core.BFloat16) = floatlayout(io, float, 8)
end

function floatlayout(io::IO, float::AbstractFloat, expbits::Int)
    fsign, fexp, fmant = NUMBER_BIT_FACES.sign, NUMBER_BIT_FACES.exponent, NUMBER_BIT_FACES.mantissa
    bitstr = bitstring(float)
    hl_bits = S"{$fsign:$(bitstr[1])}{$fexp:$(bitstr[2:expbits+1])}{$fmant:$(bitstr[expbits+2:end])}"
    if get(io, :compact, false) == true
        print(io, hl_bits)
    else
        fracbits = 8 * sizeof(float) - expbits - 1
        fracdp = round(Int, log10(2 ^ (fracbits + 1)))
        maxexp = 2^(expbits - 1) - 1
        sign = ifelse(bitstr[1] == '1', '-', '+')
        bits = reinterpret(UInt64, Float64(float))
        exponent = Int((bits >> 52) & Base.Ryu.EXP_MASK) - 1023
        fraction = reinterpret(Float64, bits & Base.Ryu.MANTISSA_MASK | 0x3ff0000000000000)
        expstr = cpad(if exponent == 1024
                          "Inf"
                      elseif exponent <= -maxexp
                          "2^$(-maxexp + 1)"
                      else "2^$exponent" end,
                      expbits - 1, ' ', RoundUp)
        fracstr = cpad(if exponent == 1024
                           ifelse(fraction == 1.0, "1", "NaN")
                       elseif exponent <= -maxexp
                           Base.Ryu.writefixed(float / floatmin(float), fracdp + 2)
                       else
                           Base.Ryu.writefixed(fraction, fracdp + 2)
                       end, fracbits, ' ', RoundUp)
        hl_info = let eleft = (expbits - 3) ÷ 2
                      eright = (expbits - 3) - eleft
                      fleft = (fracbits - 3) ÷ 2
                      fright = (fracbits - 3) - fleft
            S"{$fsign:╨}{$fexp:└$('─'^eleft)┬$('─'^eright)┘}{$fmant:└$('─'^fleft)┬$('─'^fright)┘}"
        end
        hl_vals = S"{$fsign,bold:$sign}{$fexp:$expstr}{bold:×}{$fmant:$fracstr}"
        hl_more = S"  {$fexp:exponent}$(' '^17){$fmant:mantissa / fraction}"
        fval = if hasmethod(Base.Ryu.writefixed, Tuple{typeof(float), Int})
            float
        else widen(typeof(float))(float) end
        println(io, "\n ", hl_bits, " \n ", hl_info, "\n ", hl_vals,
                S"\n {bold:=} ", if -8 < exponent < 8
                    Base.Ryu.writefixed(fval, fracdp)
                else Base.Ryu.writeexp(fval, fracdp) end)
    end
end

# ------------------
# Vector/Memory
# ------------------

function vecbytes(io::IO, items::DenseVector{T};
                  elshowfn::Function = show,
                  eltext = "item",
                  topbar::NamedTuple = (lcap='┌', lbar='╴', bar='─', rbar='╶', rcap='┐', trunc='⋯'),
                  itemfaces::Vector{Symbol} = FACE_CYCLE,
                  byteface::Symbol = :light,
                  bitcolour::Bool = false,
                  bytevals::Bool = T != UInt8) where {T}
    nitems = length(items)
    tsize, bytes = if hassizeof(T) && Base.array_subpadding(UInt8, T)
        sizeof(T), reinterpret(UInt8, items)
    else
        sizeof(Ptr), unsafe_wrap(Vector{UInt8}, Ptr{UInt8}(pointer(items)), (sizeof(Ptr) * length(items)))
    end
    nbytes = length(bytes)
    if nbytes == 1
        println(io, "\n ", bitstring(first(bytes)), "\n ", "└──────┘")
        return
    end
    margintextwidth = 4 + textwidth(eltext) + ndigits(nbytes)
    showbytes = if last(displaysize(io)) - 2 - margintextwidth >=8 * nbytes
        nbytes
    else
        (last(displaysize(io)) - 2 - ndigits(nbytes) - textwidth("⋯(×)⋯") - margintextwidth) ÷ 8
    end
    lbytes, rbytes = showbytes ÷ 2, showbytes - showbytes ÷ 2
    litems, ritems = lbytes ÷ tsize, rbytes ÷ tsize
    print(io, "\n ")
    function fmtitem(arr, idx)
        truncval = struncate(
            if isassigned(arr, idx)
                sval = sprint(elshowfn, arr[idx], context = :typeinfo => T)
                if hassizeof(T)
                    AnnotatedString(sval)
                else
                    S"{about_pointer:&}$sval"
                end
            else
                S"#undef"
            end,
            8 * tsize - 4, topbar.trunc)
        padval = cpad(topbar.lbar * truncval * topbar.rbar, 8 * tsize - 2, topbar.bar)
        tbar = topbar.lcap * padval * topbar.rcap
        face = itemfaces[mod1(idx, length(itemfaces))]
        S"{$face:$tbar}"
    end
    for litem in 1:litems
        print(io, fmtitem(items, litem))
    end
    if showbytes < nbytes
        lbar = 8 * (lbytes - litems * tsize) - 1
        facel = itemfaces[mod1(litems + 1, length(itemfaces))]
        lbar > 0 && print(io, S"{$facel:$(topbar.lcap)$(topbar.bar^lbar)}")
        print(io, S" {shadow:⋯(×$(lpad(nitems-litems-ritems, ndigits(nbytes-showbytes))))⋯} ")
        rbar = 8 * (rbytes - ritems * tsize) - 1
        facer = itemfaces[mod1(nitems - ritems, length(itemfaces))]
        rbar > 0 && print(io, S"{$facer:$(topbar.bar^rbar)$(topbar.rcap)}")
    elseif litems + ritems < nitems
        face = itemfaces[mod1(litems + 1, length(itemfaces))]
        print(io, fmtitem(items, litems + 1))
    end
    for ritem in nitems-ritems+1:nitems
        face = itemfaces[mod1(ritem, length(itemfaces))]
        print(io, fmtitem(items, ritem))
    end
    print(io, S" {emphasis:$(lpad(nitems, ndigits(nbytes)))} $eltext$(splural(nitems))")
    lbstring = AnnotatedString(join(map(bitstring, bytes[1:lbytes])))
    rbstring = AnnotatedString(join(map(bitstring, bytes[end-rbytes+1:end])))
    if bitcolour
        for b in 1:lbytes
            face!(lbstring[8*(b-1)+1:8*b], itemfaces[mod1(b, length(itemfaces))])
        end
        for b in 1:rbytes
            face!(rbstring[8*(b-1)+1:8*b], itemfaces[mod1(b + nbytes - rbytes, length(itemfaces))])
        end
    end
    for bstr in (lbstring, rbstring), (; match) in eachmatch(r"0+", bstr)
        face!(match, :shadow)
    end
    print(io, "\n ", lbstring, if showbytes < nbytes ' '^(7 + ndigits(nbytes-showbytes)) else "" end, rbstring)
    if bytevals
        print(io, ' '^ndigits(nbytes), S"  {shadow:in}\n ")
        function fmtbyte(b, _itemidx)
            S"{$byteface:└─0x$(lpad(string(b, base=16), 2, '0'))─┘}"
        end
        for b in 1:lbytes
            print(io, fmtbyte(bytes[b], fld1(b, tsize)))
        end
        showbytes < nbytes && print(io, S" {shadow:⋯(×$(nbytes-showbytes))⋯} ")
        for b in nbytes-rbytes+1:nbytes
            print(io, fmtbyte(bytes[b], fld1(b, tsize)))
        end
        print(io, S" {emphasis:$nbytes} byte$(splural(nbytes))")
    end
    println(io)
end

@static if VERSION >= v"1.11-alpha"
    addrspacelabel(::Core.AddrSpace{T}) where {T} = String(nameof(T))
    addrspacelabel(::Core.AddrSpace{Core}) = "CPU"
    addrspacelabel(::GenericMemory{_kind, _T, addrspace}) where {_kind, _T, addrspace} =
        addrspacelabel(addrspace)

    function memorylayout(io::IO, mem::GenericMemory{kind, T, addrspace}) where {kind, T, addrspace}
        if mem.length == 0
            println(io, S" {shadow:(empty)} {about_pointer:@ $(sprint(show, UInt64(mem.ptr)))}")
            return
        end
        nonptr = hassizeof(T)
        tsize = if nonptr sizeof(T) else sizeof(Ptr) end
        println(io, "\n ",
                if kind === :atomic "Atomic memory block" else "Memory block" end,
                S" ({emphasis:$(addrspacelabel(addrspace))}-addressed) \
                  from {about_pointer:$(sprint(show, UInt64(mem.ptr)))} to {about_pointer:$(sprint(show, UInt64(mem.ptr + mem.length * tsize)))}.")
        vecbytes(io, mem, eltext = if nonptr "item" else "pointer" end)
    end

    function memorylayout(io::IO, arr::Array{T}) where {T}
        invoke(memorylayout, Tuple{IO, Any}, io, arr)
        nonptr = hassizeof(T)
        tsize = if nonptr sizeof(T) else sizeof(Ptr) end
        memtypename = let alias = Base.make_typealias(fieldtype(typeof(arr), 1))
            if !isnothing(alias)
                first(alias).name
            else
                nameof(fieldtype(typeof(arr), 1))
            end
        end
        println(S"\n {julia_type:$T} contents exist on the {emphasis:$(addrspacelabel(arr.ref.mem))} \
                  within the {$(first(FACE_CYCLE)):$(fieldname(typeof(arr), 1))}{shadow:::}{$(first(FACE_CYCLE)):$memtypename} \
                  from {about_pointer:$(sprint(show, UInt64(arr.ref.mem.ptr)))} to {about_pointer:$(sprint(show, UInt64(arr.ref.mem.ptr + arr.ref.mem.length * tsize)))}.")
        vecbytes(io, arr, eltext = if nonptr "item" else "pointer" end)
    end
end

# ------------------
# Symbols (interned)
# ------------------

function about(io::IO, value::Symbol)
    println(io, S"Symbol ({julia_comparator:<:} {julia_type:Any}), an {about_bytes:$(sizeof(Ptr))B} reference to a {about_bytes:$(sizeof(value))B} interned string")
end

# ------------------
# Char/String
# ------------------

function memorylayout(io::IO, char::Char)
    chunks = @static if VERSION >= v"1.10"
        reinterpret(NTuple{4, UInt8}, reinterpret(UInt32, char) |> hton)
    else
        Tuple(reinterpret(UInt8, [reinterpret(UInt32, char) |> hton]))
    end
    get(io, :compact, false) || print(io, "\n ")
    nchunks = something(findlast(!iszero, chunks), 1)
    byte0leading = [1, 3, 4, 5][nchunks]
    ucodepoint = if Base.isoverlong(char)
        Base.decode_overlong(char)
    else
        codepoint(char)
    end
    bit_spreads =
        [[3, 4],
         [3, 4, 4],
         [4, 4, 4, 4],
         [1, 4, 4, 4, 4, 4]
         ][nchunks]
    ubytes = collect(uppercase(string(
        ucodepoint, base=16, pad = length(bit_spreads))))
    overlong_bytes = if Base.isoverlong(char)
        1:min(something(findfirst(==('1'), ubytes), length(ubytes)) - 1,
              length(ubytes) - 2)
    else 1:0 end
    chunk_coloring = [Pair{UnitRange{Int}, Symbol}[] for _ in 1:length(chunks)]
    ustr = S"{bold:U+$(lpad(join(ubytes), 4, '0'))}"
    for (i, b, color) in zip(1:length(ubytes),
                             collect(eachindex(ustr))[end-length(ubytes)+1:end],
                             Iterators.cycle(Iterators.reverse(FACE_CYCLE)))
        if i in overlong_bytes
            color = :error # overlong
        end
        face!(ustr[b:b], color)
    end
    if get(io, :compact, false) == true
        print(io, ustr, ' ')
    else
        current_bit = byte0leading
        print(io, ' '^byte0leading)
        for (i, ubyte, nbits, color) in zip(1:length(ubytes), ubytes, bit_spreads,
                                            Iterators.cycle(Iterators.reverse(FACE_CYCLE)))
            if i in overlong_bytes
                color = :error # overlong
            end
            does_byte_jump = current_bit ÷ 8 < (current_bit + nbits) ÷ 8
            clean_jump = does_byte_jump && (current_bit + nbits) % 8 == 0
            next_bit = current_bit + nbits + does_byte_jump * 2
            width = nbits + 3 * (does_byte_jump && !clean_jump)
            byte_brace = if width <= 2
                lpad(ubyte, width)
            else
                '┌' * cpad(ubyte, width-2, '─') * '┐'
            end
            print(io, S"{$color:$byte_brace}")
            clean_jump && print(io, "   ")
            if does_byte_jump && !clean_jump
                push!(chunk_coloring[1 + current_bit ÷ 8], (1 + current_bit % 8):8 => color)
                push!(chunk_coloring[1 + next_bit ÷ 8],    3:mod1(next_bit, 8) => color)
            else
                push!(chunk_coloring[1 + current_bit ÷ 8],
                      (1 + current_bit % 8):mod1(current_bit + nbits, 8) => color)
            end
            current_bit = next_bit
        end
        print(io, "\n ")
    end
    for (i, (chunk, coloring)) in enumerate(zip(chunks, chunk_coloring))
        cbits = bitstring(chunk)
        cstr = if i > nchunks
            S"{shadow:$cbits}"
        else
            leadingbits = if i == 1; byte0leading else 2 end
            leading = cbits[1:leadingbits]
            rest = AnnotatedString(cbits[leadingbits+1:end])
            for (; match) in eachmatch(r"1+", rest)
                face!(match, :underline)
            end
            cstr = S"{shadow:$leading}$rest"
            for (range, color) in coloring
                face!(cstr, range, color)
            end
            cstr
        end
        print(io, cstr, ' ')
    end
    if get(io, :compact, false) != true
        println(io)
        for chunk in chunks
            byte = lpad(string(chunk, base=16), 2, '0')
            print(io, S" {shadow:└─0x$(byte)─┘}")
        end
        print(io, "\n = ", ustr)
        Base.isoverlong(char) && print(io, S" {error:[overlong]}")
        println(io)
    end
end

const CONTROL_CHARACTERS =
    ('\x00' => ("NULL", "Null character",              "Originally the code of blank paper tape and used as padding to slow transmission. Now often used to indicate the end of a string in C-like languages."),
     '\x01' => ("SOH",  "Start of Heading",            ""),
     '\x02' => ("SOT",  "Start of Text",               ""),
     '\x03' => ("ETX",  "End of Text",                 ""),
     '\x04' => ("EOT",  "End of Transmission",         ""),
     '\x05' => ("ENQ",  "Enquiry",                     "Trigger a response at the receiving end, to see if it is still present."),
     '\x06' => ("ACK",  "Acknowledge",                 "Indication of successful receipt of a message."),
     '\x07' => ("BEL",  "Bell",                        "Call for attention from an operator."),
     '\x08' => ("HBS",  "Backspace",                   "Move one position leftwards. Next character may overprint or replace the character that was there."),
     '\x09' => ("HT",   "Horizontal Tab",              "Move right to the next tab stop."),
     '\x0a' => ("LF",   "Line Feed",                   "Move down to the same position on the next line (some devices also moved to the left column)."),
     '\x0b' => ("VT",   "Vertical Tab",                "Move down to the next vertical tab stop. "),
     '\x0c' => ("FF",   "Form Feed",                   "Move down to the top of the next page. "),
     '\x0d' => ("CR",   "Carriage Return",             "Move to column zero while staying on the same line."),
     '\x0e' => ("SO",   "Shift Out",                   "Switch to an alternative character set."),
     '\x0f' => ("SI",   "Shift In",                    "Return to regular character set after SO."),
     '\x10' => ("DLE",  "Data Link Escape",            "Cause a limited number of contiguously following characters to be interpreted in some different way."),
     '\x11' => ("DC1",  "Device Control One (XON)",    "Used by teletype devices for the paper tape reader and tape punch. Became the de-facto standard for software flow control, now obsolete."),
     '\x12' => ("DC2",  "Device Control Two",          "Used by teletype devices for the paper tape reader and tape punch. Became the de-facto standard for software flow control, now obsolete."),
     '\x13' => ("DC3",  "Device Control Three (XOFF)", "Used by teletype devices for the paper tape reader and tape punch. Became the de-facto standard for software flow control, now obsolete."),
     '\x14' => ("DC4",  "Device Control Four",         "Used by teletype devices for the paper tape reader and tape punch. Became the de-facto standard for software flow control, now obsolete."),
     '\x15' => ("NAK",  "Negative Acknowledge",        "Negative response to a sender, such as a detected error. "),
     '\x16' => ("SYN",  "Synchronous Idle",            "A transmission control character used by a synchronous transmission system in the absence of any other character (idle condition) to provide a signal from which synchronism may be achieved or retained between data terminal equipment."),
     '\x17' => ("ETB",  "End of Transmission Block",   "End of a transmission block of data when data are divided into such blocks for transmission purposes."),
     '\x18' => ("CAN",  "Cancel",                      "A character, or the first character of a sequence, indicating that the data preceding it is in error. As a result, this data is to be ignored. The specific meaning of this character must be defined for each application and/or between sender and recipient."),
     '\x19' => ("EM",   "End of Medium",               "Indicates on paper or magnetic tapes that the end of the usable portion of the tape had been reached."),
     '\x1a' => ("SUB",  "Substitute/Control-Z",        "A control character used in the place of a character that has been found to be invalid or in error. SUB is intended to be introduced by automatic means."),
     '\x1b' => ("ESC",  "Escape",                      "A control character which is used to provide additional control functions. It alters the meaning of a limited number of contiguously following bit combinations. The use of this character is specified in ISO-2022."),
     '\x1c' => ("FS",   "File Separator",              "Used to separate and qualify data logically; its specific meaning has to be specified for each application. If this character is used in hierarchical order, it delimits a data item called a file. "),
     '\x1d' => ("GS",   "Group Separator",             "Used to separate and qualify data logically; its specific meaning has to be specified for each application. If this character is used in hierarchical order, it delimits a data item called a group."),
     '\x1e' => ("RG",   "Record Separator",            "Used to separate and qualify data logically; its specific meaning has to be specified for each application. If this character is used in hierarchical order, it delimits a data item called a record."),
     '\x1f' => ("US",   "Unit Separator",              "Used to separate and qualify data logically; its specific meaning has to be specified for each application. If this character is used in hierarchical order, it delimits a data item called a unit."),
     '\x7f' => ("DEL",  "Delete",                      "Originally used to delete characters on punched tape by punching out all the holes."))

function elaboration(io::IO, char::Char)
    c0index = findfirst(c -> first(c) == char, CONTROL_CHARACTERS)
    stychr = S"{julia_char:$(sprint(show, char))}"
    if !isnothing(c0index)
        cshort, cname, cinfo = last(CONTROL_CHARACTERS[c0index])
        println(io, "\n Control character ", stychr, ": ", cname, " ($cshort)",
                ifelse(isempty(cinfo), "", "\n "), cinfo)
    elseif isascii(char)
        kind = if char in 'a':'z'
            "lowercase letter"
        elseif char in 'A':'Z'
            "uppercase letter"
        elseif char in '0':'9'
            "numeral"
        elseif char == ' '
            "space"
        elseif char in ('(', ')', '[', ']', '{', '}', '«', '»')
            "parenthesis"
        elseif char in ('!':'/'..., ':':'@'..., '\\', '^', '_', '`', '|', '~')
            "punctuation"
        end
        println(io, "\n ASCII $kind ", stychr)
    elseif char in ('Ç':'ø'..., 'Ø', 'á':'Ñ'..., 'Á':'À', 'ã', 'Ã', 'ð':'Ï'..., 'Ó':'Ý')
        println(io, "\n Extended ASCII accented letter ", stychr,
                S" ({julia_number:0x$(string(UInt8(char), base=16))})")
    elseif Base.isoverlong(char)
    elseif codepoint(char) in 128:255
        println(io, "\n Extended ASCII symbol ", stychr,
                S" ({shadow:0x$(string(Int(char), base=16))})")
    else
        catstr = Base.Unicode.category_string(char)
        catabr = Base.Unicode.category_abbrev(char)
        println(io, S"\n Unicode $stychr, category: $catstr ($catabr)")
    end
end

function elaboration(io::IO, str::String)
    charfreq = Dict{Char, Int}()
    for char in str
        charfreq[char] = get(charfreq, char, 0) + 1
    end
    charset_index =
        maximum(c -> if Int(c) < 127; 2
                elseif Int(c) < 255; 3
                else 4 end,
                keys(charfreq),
                init = 1)
    charset = ["None", "ASCII", "Extended ASCII", "Unicode"][charset_index]
    println(io, S" {emphasis:•} Character set: {emphasis:$charset}")
    control_character_counts = map(
        c -> get(charfreq, first(c), 0), CONTROL_CHARACTERS)
    if !all(iszero, control_character_counts)
        println(io, S" {emphasis:•} Contains \
          {about_count:$(sum(control_character_counts))} \
          instances of {about_count:$(sum(>(0), control_character_counts))} \
          control characters:")
        for ((char, info), count) in zip(CONTROL_CHARACTERS, control_character_counts)
            count > 0 &&
                println(io, S"   {emphasis:∗} {julia_char:$(sprint(show, char))} ({about_count:$count}): $(join(info, ' '))")
        end
    end
    if startswith(str, '\ufeff')
        println(io, S" {emphasis:•} Prefixed by BOM (byte-order mark)")
    end
    vecbytes(io, codeunits(str),
             eltext = "codepoint", elshowfn = (io, c) -> show(io, Char(c)),
             bitcolour = true, bytevals = false)
end

# TODO struct
