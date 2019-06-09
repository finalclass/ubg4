-module(ubg4).

-export([build/0]).
-export([tt/0]).

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
    build_module(ubg4_search_handler),
    build_module(ubg4_not_found_handler),
    build_module(ubg4_data_sup),
    build_module(ubg4_projector_data_sup).
    
tt() ->
    ubg4_data:find_verse(<<"rdz 1:2">>).
