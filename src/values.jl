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
        println(io, S"{italic:singelton}")
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
    namewidth = maximum(textwidth, fnames)
    typewidth = min(maximum(textwidth, ftypes), width ÷ 4)
    sizewidth = maximum(textwidth, fsizes)
    width -= 1 + namewidth + 1 + typewidth + 2 + sizewidth
    reprwidth = min((2 * width) ÷ 3, maximum(textwidth, freprs))
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
                S" {bold:=} {$(NUMBER_BIT_FACES.sign):$signstr}$(abs(value))")
    end
end

memorylayout(io::IO, float::Float64) = floatlayout(io, float, 11)
memorylayout(io::IO, float::Float32) = floatlayout(io, float, 8)
memorylayout(io::IO, float::Float16) = floatlayout(io, float, 5)
memorylayout(io::IO, float::Core.BFloat16) = floatlayout(io, float, 8)

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
            S"{$fsign:╨}{$fexp:└$('─'^eleft)┬$('─'^eright)┘}{$fmant:└$('─'^fleft)┬$('─'^fright)┘}"
        end
        hl_vals = S"{$fsign,bold:$sign}{$fexp:$expstr}{bold:×}{$fmant:$fracstr}"
        hl_more = S"  {$fexp:exponent}$(' '^17){$fmant:mantissa / fraction}"
        println(io, "\n ", hl_bits, " \n ", hl_info, "\n ", hl_vals,
                S"\n {bold:=} ", if -8 < exponent < 8
                    Base.Ryu.writefixed(float, fracdp)
                else Base.Ryu.writeexp(float, fracdp) end)
    end
end

# ------------------
# Char/String
# ------------------

function memorylayout(io::IO, char::Char)
    chunks = reinterpret(NTuple{4, UInt8}, reinterpret(UInt32, char) |> hton)
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
    else let
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
    end end
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

    end
end

# TODO struct
