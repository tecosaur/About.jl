const NUMBER_BIT_FACES = (
    sign = :bright_blue,
    exponent = :bright_green,
    mantissa = :bright_red
)

function about(io::IO, value::T) where {T}
    print(io, Base.summary(value))
    ismutable(value) && print(io, " (mutable)")
    directbytes = sizeof(value)
    indirectbytes = Base.summarysize(value)
    print(io, styled", {bold:$(join(humansize(directbytes)))}")
    if indirectbytes > directbytes
        print(io, styled" referencing {bold:$(join(humansize(indirectbytes)))}")
    end
    print(io, styled" ({julia_comparator:<:} ", supertypestr(supertype(T)), ")")
    println(io)
    memorylayout(io, value)
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
        println(io, styled"{italic:singelton}")
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
        push!(ffaces, face)
        push!(fnames, string(name))
        push!(ftypes, string(type))
        push!(fsizes, join(humansize(size)))
        aio = AnnotatedIOBuffer()
        if Base.issingletontype(type)
            push!(freprs, styled"{shadow:singleton}")
        elseif size == 0
            push!(freprs, styled"{error:??}")
        elseif ispointer
            try
                pt = pointer(getfield(value, name))
                push!(freprs, styled"{$POINTER_FACE,light:$pt}")
            catch
                push!(freprs, styled"{$POINTER_FACE:Ptr?}")
            end
        else
            memorylayout(IOContext(aio, :compact => true), getfield(value, name))
            push!(freprs, read(seekstart(aio), AnnotatedString))
        end
        truncate(aio, 0)
        show(IOContext(aio, :compact => true), getfield(value, name))
        push!(fshows, read(seekstart(aio), AnnotatedString))
    end
    width = last(displaysize(io)) - 2
    namewidth = maximum(textwidth, fnames)
    typewidth = min(maximum(textwidth, ftypes), width ÷ 4)
    sizewidth = maximum(textwidth, fsizes)
    width -= 1 + namewidth + 1 + typewidth + 2 + sizewidth
    reprwidth = min((2 * width) ÷ 3, maximum(textwidth, freprs))
    showwidth = width - reprwidth
    for (face, name, type, size, brepr, shown) in zip(ffaces, fnames, ftypes, fsizes, freprs, fshows)
        println(io, ' ',
                styled"{$face:$(lpad(name, namewidth)){shadow:::}$(rpad(struncate(type, typewidth, \"…\", :right), typewidth)) $(lpad(size, sizewidth))}",
                ' ', rpad(struncate(brepr, reprwidth, styled" {shadow:…} "), reprwidth),
                ' ', face!(struncate(shown, showwidth, styled" {shadow:…} "), face))
    end
    println(io)
    memorylayout(io, T)
end

function memorylayout(io::IO, value::Bool)
    bits = AnnotatedString(bitstring(value))
    face!(bits[1:end-1], :shadow)
    face!(bits[end:end], NUMBER_BIT_FACES.sign)
    if get(io, :compact, false) == true
        print(io, bits)
    else
        println(io, "\n ", bits, styled" {bold:=} $value")
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
                styled" {bold:=} $value")
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
                styled" {bold:=} {$(NUMBER_BIT_FACES.sign):$signstr}$value")
    end
end

memorylayout(io::IO, float::Float64) = floatlayout(io, float, 11)
memorylayout(io::IO, float::Float32) = floatlayout(io, float, 8)
memorylayout(io::IO, float::Float16) = floatlayout(io, float, 5)
memorylayout(io::IO, float::Core.BFloat16) = floatlayout(io, float, 8)

function floatlayout(io::IO, float::AbstractFloat, expbits::Int)
    fsign, fexp, fmant = NUMBER_BIT_FACES.sign, NUMBER_BIT_FACES.exponent, NUMBER_BIT_FACES.mantissa
    bitstr = bitstring(float)
    hl_bits = styled"{$fsign:$(bitstr[1])}{$fexp:$(bitstr[2:expbits+1])}{$fmant:$(bitstr[expbits+2:end])}"
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
                      else "2^$exponent" end,
                      expbits - 1, ' ', RoundUp)
        fracstr = cpad(if exponent == 1024
                           ifelse(fraction == 1.0, "1", "NaN")
                       else
                           Base.Ryu.writefixed(fraction, fracdp + 2)
                       end, fracbits, ' ', RoundUp)
        hl_info = let eleft = (expbits - 3) ÷ 2
                      eright = (expbits - 3) - eleft
                      fleft = (fracbits - 3) ÷ 2
                      fright = (fracbits - 3) - fleft
            styled"{$fsign:╨}{$fexp:└$('─'^eleft)┬$('─'^eright)┘}{$fmant:└$('─'^fleft)┬$('─'^fright)┘}"
        end
        hl_vals = styled"{$fsign,bold:$sign}{$fexp:$expstr}{bold:×}{$fmant:$fracstr}"
        hl_more = styled"  {$fexp:exponent}$(' '^17){$fmant:mantissa / fraction}"
        println(io, "\n ", hl_bits, " \n ", hl_info, "\n ", hl_vals,
                styled"\n {bold:=} ", if -8 < exponent < 8
                    Base.Ryu.writefixed(float, fracdp)
                else Base.Ryu.writeexp(float, fracdp) end)
    end
end

function memorylayout(io::IO, char::Char)
    chunks = reinterpret(NTuple{4, UInt8}, reinterpret(UInt32, char) |> hton)
    get(io, :compact, false) || print(io, "\n ")
    for chunk in chunks
        cstr = AnnotatedString(bitstring(chunk))
        if iszero(chunk)
            face!(cstr, :shadow)
        else
            for (; match) in eachmatch(r"1+", cstr)
                face!(match, :bright_green)
            end
        end
        print(io, cstr, ' ')
    end
    if get(io, :compact, false) != true
        println(io)
        for chunk in chunks
            byte = lpad(string(chunk, base=16), 2, '0')
            print(io, styled" {shadow:└─0x$(byte)─┘}")
        end
        println(io)
    end
end

# TODO char

# TODO struct
