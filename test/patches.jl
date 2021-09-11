# NOTE: this file keeps patches for julia itself to make JET.jl test keep passing

# TODO remove this patch once https://github.com/aviatesk/JET.jl/issues/184 is resolved
@static if VERSION ≥ v"1.8.0-DEV.421"
    @eval Base mapreduce_empty(f, op, T) = _empty_reduce_error()
end
