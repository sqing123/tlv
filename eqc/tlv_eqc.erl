%% -----------------------------------------------------------------------------
%% QuickCheck test for tlv encoder
%% See http://www.quviq.com/products/erlang-quickcheck/
%% -----------------------------------------------------------------------------
-module(tlv_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% Generators
gen_atom() ->
    ?LET(I, eqc_gen:int(), list_to_atom(integer_to_list(I))).


gen_map_value() ->
    oneof([gen_atom(), eqc_gen:int(), eqc_gen:binary()]).


gen_map() ->
    eqc_gen:map(gen_atom(),
                oneof([gen_map_value(), eqc_gen:map(gen_atom(), gen_map_value())])).

%% Property
prop_can_encode_decode() ->
    ?FORALL(V, oneof([eqc_gen:binary(), eqc_gen:int(), gen_atom(), gen_map()]),
            V == tlv:decode(tlv:encode(V))
           ).