-module(ubg4).

-export([build/0]).

build_module(Mod) ->
    code:purge(Mod),
    compile:file("src/" ++ atom_to_list(Mod)),
    code:load_file(Mod).

build() ->
    erlydtl:compile_file("./priv/index.dtl", ubg4_templates_index),
    build_module(ubg4_app),
    build_module(ubg4_data),
    build_module(ubg4_handler),
    build_module(ubg4_sup).
    
