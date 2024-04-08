function about(io::IO, fn::Function)
    source = Main.InteractiveUtils.which(parentmodule(fn), Symbol(fn))
    methodmodules = getproperty.(methods(fn).ms, :module)
    others = setdiff(methodmodules, [source])
    fn_name, fn_extra = split(Base.summary(fn), ' ', limit=2)
    print(io, styled"{julia_funcall:$fn_name} $fn_extra\n Defined in {about_module:$source}")
    if length(others) > 0
        print(io, styled"{shadow:({emphasis:$(sum(Ref(source) .=== methodmodules))})} extended in ")
        for (i, oth) in enumerate(others)
            print(io, styled"{about_module:$oth}{shadow:({emphasis:$(sum(Ref(oth) .=== methodmodules))})}")
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
end

function about(io::IO, @nospecialize(cfn::ComposedFunction))
    print(io, styled"{bold:Composed function:} ")
    fnstack = Function[]
    function decompose!(fnstk, c::ComposedFunction)
        decompose!(fnstk, c.outer)
        decompose!(fnstk, c.inner)
    end
    decompose!(fnstk, c::Function) = push!(fnstk, c)
    decompose!(fnstack, cfn)
    join(io, map(f -> styled"{julia_funcall:$f}", fnstack), styled" {julia_operator:∘} ")
    println(io)
    for fn in fnstack
        print(io, styled" {emphasis:•} ")
        about(io, fn)
    end
end

function about(io::IO, method::Method)
    fn, sig = first(method.sig.types).instance, Tuple{map(Base.unwrap_unionall, method.sig.types[2:end])...}
    show(io, method)
    println(io)
    print_effects(io, fn, sig)
end

function about(io::IO, fn::Function, @nospecialize(sig::Type{<:Tuple}))
    about(io, fn); println(io)
    ms = methods(fn, sig)
    if isempty(ms)
        fncall = highlight("$fn($(join(collect(sig.types), ", ")))")
        println(io, styled" {error:!} No methods matched $fncall")
        return
    end
    println(io, styled" Matched {emphasis:$(length(ms))} method$(ifelse(length(ms) > 1, \"s\", \"\")):")
    for method in ms
        println(io, "  ", sprint(show, method, context=IOContext(io)))
    end
    print_effects(io, fn, sig)
end

function print_effects(io::IO, fn::Function, @nospecialize(sig::Type{<:Tuple}))
    effects = Base.infer_effects(fn, sig)
    ATRUE, AFALSE = Core.Compiler.ALWAYS_TRUE, Core.Compiler.ALWAYS_FALSE
    echar(t::UInt8) = get(Dict(ATRUE => '✔', AFALSE => '✗'), t, '?')
    echar(b::Bool) = ifelse(b, '✔', '✗')
    eface(t::UInt8) = get(Dict(ATRUE => :success, AFALSE => :error), t, :warning)
    eface(b::Bool) = ifelse(b, :success, :error)
    hedge(t::UInt8) = get(Dict(ATRUE => styled"guaranteed to",
                               AFALSE => styled"{italic:not} guaranteed to"), t,
                          styled"???")
    hedge(b::Bool) = hedge(ifelse(b, ATRUE, AFALSE))
    println(io)
    for (effect, description) in
        [(:consistent, "return or terminate consistently"),
         (:effect_free, "be free from externally semantically visible side effects"),
         (:nothrow, "never throw an exception"),
         (:terminates, "terminate")]
        e = getfield(effects, effect)
        print(io, styled" {$(eface(e)):{bold:$(echar(e))} $(rpad(effect, 11))}  {shadow:$(hedge(e)) $description}")
        print('\n')
    end
end

about(io::IO, fn::Function, sig::NTuple{N, <:Type}) where {N} = about(io, fn, Tuple{sig...})
about(io::IO, fn::Function, sig::Type...) = about(io, fn, sig)
