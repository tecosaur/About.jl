# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

function about(io::IO, fn::Function)
    methodmodules = [nameof(m.module) for m in methods(fn).ms]
    source, others = if startswith(String(nameof(fn)), '#') && length(methodmodules) == 1
        first(methodmodules), Symbol[]
    else
        nameof(Main.InteractiveUtils.which(parentmodule(fn), nameof(fn))), setdiff(methodmodules, [source])
    end
    fn_smry = split(Base.summary(fn), ' ', limit=2)
    fn_name, fn_extra = if length(fn_smry) == 1 (fn_smry[1], "") else fn_smry end
    print(io, S"{julia_funcall:$fn_name} $fn_extra\n Defined in {about_module:$source}")
    if length(others) > 0
        print(io, S"{shadow:({emphasis:$(sum(Ref(source) .=== methodmodules))})} extended in ")
        for (i, oth) in enumerate(others)
            print(io, S"{about_module:$oth}{shadow:({emphasis:$(sum(Ref(oth) .=== methodmodules))})}")
            if length(others) == 2 && i == 1
                print(io, " and ")
            elseif length(others) > 2 && i < length(others)-1
                print(io, ", ")
            elseif length(others) > 2 && i == length(others)-1
                print(io, ", and ")
            end
        end
    end
    print(io, ".\n")
    if !get(io, :About_inner, false)
        println(io, S"\n {tip:■ Hint:} {grey:to get more information on a particular method try} {light:$(highlight(\"about($fn, argtypes...)\"))}")
    end
end

function about(io::IO, @nospecialize(cfn::ComposedFunction))
    print(io, S"{bold:Composed function:} ")
    fnstack = Function[]
    function decompose!(fnstk, c::ComposedFunction)
        decompose!(fnstk, c.outer)
        decompose!(fnstk, c.inner)
    end
    decompose!(fnstk, c::Function) = push!(fnstk, c)
    decompose!(fnstack, cfn)
    join(io, map(f -> S"{julia_funcall:$f}", fnstack), S" {julia_operator:∘} ")
    println(io)
    for fn in fnstack
        print(io, S" {emphasis:•} ")
        about(IOContext(io, :About_inner => true), fn)
    end
end

function about(io::IO, method::Method)
    fn, sig = first(method.sig.types).instance, Tuple{map(Base.unwrap_unionall, method.sig.types[2:end])...}
    show(io, method)
    println(io)
    print_effects(io, fn, sig)
end

function about(io::IO, fn::Union{Function, Type}, @nospecialize(argtypes::Type{<:Tuple}))
    iio = IOContext(io, :About_inner => true)
    about(iio, fn); println(io)
    ms = methods(fn, argtypes)
    if isempty(ms)
        fncall = highlight("$fn($(join(collect(argtypes.types), ", ")))")
        println(io, S" {error:!} No methods matched $fncall")
        return
    end
    rinfo = let rtypes = Base.return_types(fn, argtypes) # HACK: this is technically private API
        unique!(rtypes)
        for i in eachindex(rtypes), j in eachindex(rtypes)
            Tᵢ, Tⱼ = rtypes[i], rtypes[j]
            if Tᵢ <: Tⱼ
                rtypes[i] = Tⱼ
            elseif Tⱼ <: Tᵢ
                rtypes[j] = Tᵢ
            end
        end
        unique!(rtypes)
        sort!(rtypes, by=length ∘ supertypes)
        join(map(t -> S"{julia_type:$t}", rtypes), ", ")
    end
    println(io, S" Matched {emphasis:$(length(ms))} method$(ifelse(length(ms) > 1, \"s\", \"\")) {julia_type:::} $rinfo")
    for method in ms
        mcall, msrc = split(sprint(show, method), " @ ")
        msrcinfo = match(r"^([A-Z][A-Za-z0-9\.]+) (.+)$", msrc)
        msrcpretty = if isnothing(msrcinfo)
            S"{shadow,underline:$msrc}"
        else
            mmod, mfile = msrcinfo.captures
            S"{about_module:$mmod} {shadow,underline:$mfile}"
        end
        println(io, S"  $(highlight(mcall)) {shadow,bold:@} $msrcpretty")
    end
    println(io)
    about(iio, Base.infer_effects(fn, argtypes))
end

struct CompatibleCoreCompilerConstants end

function Base.getproperty(::CompatibleCoreCompilerConstants, name::Symbol)
    if isdefined(Core.Compiler, name)
        getglobal(Core.Compiler, name)
    end
end

const C4 = CompatibleCoreCompilerConstants()

function about(io::IO, effects::Core.Compiler.Effects)
    function effectinfo(io::IO, field::Symbol, name::String, labels::Pair{<:Union{UInt8, Bool, Nothing}, <:AnnotatedString{String}}...;
                        prefix::AbstractString = "", suffix::AbstractString = "")
        hasproperty(effects, field) || return
        value = getproperty(effects, field)
        icon, accent = if value === C4.ALWAYS_TRUE || value === true
            '✔', :success
        elseif value === C4.ALWAYS_FALSE || value === false
            '✗', :error
        else
            '~', :warning
        end
        msg = S"{bold,italic,grey:???}"
        for (id, label) in labels
            if id == value
                msg = label
                break
            end
        end
        name_pad_width = 13
        dispwidth = last(displaysize(io))
        declr = S" {bold,$accent:$icon $(rpad(name, name_pad_width))}  "
        get(io, :About_inner, false) === true && print(io, ' ')
        print(io, declr)
        indent = name_pad_width + 5
        desc = S"{grey:$prefix$(ifelse(isempty(prefix), \"\", \" \"))$msg$(ifelse(isempty(suffix), \"\", \" \"))$suffix}"
        desclines = wraplines(desc, dispwidth - indent, indent)
        for (i, line) in enumerate(desclines)
            i > 1 && print(io, ' '^indent)
            println(io, line)
        end
    end
    get(io, :About_inner, false) === true && print(io, ' ')
    println(io, S"{bold:Method effects}")
    effectinfo(io, :consistent, "consistent",
                C4.ALWAYS_TRUE => S"guaranteed to",
                C4.ALWAYS_FALSE => S"{italic:might} not",
                C4.CONSISTENT_IF_NOTRETURNED => S"when the return value {italic:never} involves newly allocated mutable objects, will",
                C4.CONSISTENT_IF_INACCESSIBLEMEMONLY => S"when {code:inaccessible memory only} is also proven, will",
                suffix = "return or terminate consistently")
    effectinfo(io, :effect_free, "effect free",
                C4.ALWAYS_TRUE => S"guaranteed to be",
                C4.ALWAYS_FALSE => S"{italic:might} not be",
                C4.EFFECT_FREE_IF_INACCESSIBLEMEMONLY => S"when {code:inaccessible memory only} is also proven, is",
                suffix = "free from externally semantically visible side effects")
    effectinfo(io, :nothrow, "no throw",
                true => S"guaranteed to {italic:never}",
                false => S"{italic:may}",
                suffix = "throw an exception")
    effectinfo(io, :terminates, "terminates",
                true => S"guaranteed to",
                false => S"{italic:might} not",
                suffix = "always terminate")
    effectinfo(io, :notaskstate, "no task state",
                true => S"guaranteed not to access task state (allowing migration between tasks)",
                false => S"{italic:may} access task state (preventing migration between tasks)")
    effectinfo(io, :inaccessiblememonly, "inaccessible memory only",
                C4.ALWAYS_TRUE => S"guaranteed to {italic:never} access or modify externally accessible mutable memory",
                C4.ALWAYS_FALSE => S"{italic:may} access or modify externally accessible mutable memory",
                C4.INACCESSIBLEMEM_OR_ARGMEMONLY => S"{italic:may} access or modify mutable memory {italic:iff} pointed to by its call arguments")
    effectinfo(io, :noub, "no undefined behaviour",
                C4.ALWAYS_TRUE => S"guaranteed to {italic:never}",
                C4.ALWAYS_FALSE => S"{italic:may}",
                C4.NOUB_IF_NOINBOUNDS => S"so long as {code,julia_macro:@inbounds} is not used or propagated, will not",
                suffix = "execute undefined behaviour")
    effectinfo(io, :nonoverlayed, "non-overlayed",
                true => S"{italic:never} calls any methods from an overlayed method table",
                false => S"{italic:may} call methods from an overlayed method table")
    effectinfo(io, :nortcall, "no return_type call",
                true => S"{italic:never} calls {code:Core.Compiler.return_type}",
                false => S"{italic:may} call {code:Core.Compiler.return_type}")
end

about(io::IO, fn::Union{Function, Type}, sig::NTuple{N, <:Type}) where {N} = about(io, fn, Tuple{sig...})
about(io::IO, fn::Union{Function, Type}, sig::Type...) = about(io, fn, sig)
