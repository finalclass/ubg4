-module(ubg4).

-export([build/0]).

build_module(Mod) ->
    code:purge(Mod),
    compile:file("src/" ++ atom_to_list(Mod)),
    code:load_file(Mod).

build() ->
    erlydtl:compile_dir("./priv/templates", ubg4_templates),
    build_module(ubg4_app),
    build_module(ubg4_data),
    build_module(ubg4_handler),
    build_module(ubg4_projector_handler),
    build_module(ubg4_not_found_handler),
    build_module(ubg4_sup).
    
