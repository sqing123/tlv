-module(tlv).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(INTEGER, 0).
-define(ATOM, 1).
-define(BINARY, 2).
-define(MAP, 3).

-export([encode/1, decode/1]).

%% -----------------------------------------------------------------------------

encode(Num) when is_integer(Num) ->
    Bin = integer_to_binary(Num, 10),
    Len = bit_size(Bin),
    <<?INTEGER:4, Len:16, Bin/bitstring>>;
encode(Atom) when is_atom(Atom) ->
    Bin = atom_to_binary(Atom, latin1),
    Len = bit_size(Bin),
    <<?ATOM:4, Len:16, Bin/bitstring>>;
encode(Binary) when is_binary(Binary) ->
    Len = bit_size(Binary),
    <<?BINARY:4, Len:16, Binary/bitstring>>;
encode(Map) when is_map(Map) ->
    Bin = encode_map(maps:to_list(Map), <<>>),
    Len = bit_size(Bin),
    <<?MAP:4, Len:16, Bin/bitstring>>.

encode_map([], Result) ->
    Result;
encode_map([{X,Y}|T], Result) ->
    Bin1 = encode(X),
    Bin2 = encode(Y),
    NewBinary = <<Result/bitstring, Bin1/bitstring, Bin2/bitstring>>,
    encode_map(T, NewBinary).

decode(Input) ->
    {Answer, <<>>} = decode_helper(Input),
    Answer.

decode_helper(<<?INTEGER:4, Len:16, Rest/bitstring>>) ->
    <<Num:Len/bitstring, NewRest/bitstring>> = Rest, 
    {binary_to_integer(Num), NewRest};
decode_helper(<<?ATOM:4, Len:16, Rest/bitstring>>) -> 
    <<Atom:Len/bitstring, NewRest/bitstring>> = Rest,
    {binary_to_atom(Atom, latin1), NewRest};
decode_helper(<<?BINARY:4, Len:16, Rest/bitstring>>) -> 
    <<Binary:Len/bitstring, NewRest/bitstring>> = Rest,
    {Binary, NewRest};
decode_helper(<<?MAP:4, Len:16, Rest/bitstring>>) -> 
    <<Map:Len/bitstring, NewRest/bitstring>> = Rest,
    {decode_map(Map, #{}), NewRest};
decode_helper(_) ->
    {error, not_binary}.

decode_map(<<>>, Map) ->
    Map;
decode_map(X, Map) ->
    {Key, Rest} = decode_helper(X),
    {Value, NewRest} = decode_helper(Rest),
    NewMap = maps:put(Key, Value, Map),
    decode_map(NewRest, NewMap).

%% -----------------------------------------------------------------------------

-ifdef(EUNIT).

    encode_decode_test() ->
        Num1 = 1,
        ?assertEqual(Num1, decode(encode(Num1))),
        Num2 = 246,
        ?assertEqual(Num2, decode(encode(Num2))),
        Atom1 = hello,
        ?assertEqual(Atom1, decode(encode(Atom1))),
        Atom2 = hello,
        ?assertEqual(Atom2, decode(encode(Atom2))),
        Atom3 = hello,
        ?assertEqual(Atom3, decode(encode(Atom3))),
        Atom4 = hello,
        ?assertEqual(Atom4, decode(encode(Atom4))),
        Binary1 = <<"Hello">>,
        ?assertEqual(Binary1, decode(encode(Binary1))),
        Binary2 = <<"1234">>,
        ?assertEqual(Binary2, decode(encode(Binary2))),
        Binary3 = <<"78910">>,
        ?assertEqual(Binary3, decode(encode(Binary3))),
        Binary4 = <<"GGGG">>,
        ?assertEqual(Binary4, decode(encode(Binary4))),
        Map1 = #{a=>2, b=>3, c=>4},
        ?assertEqual(Map1, decode(encode(Map1))),
        Map2 = #{c=>home, d=>hello, e=>come},
        ?assertEqual(Map2, decode(encode(Map2))),
        Map3 = #{home=>#{jeez=><<"UMBRELLA">>}, some=>3, one=>4},
        ?assertEqual(Map3, decode(encode(Map3))),
        Map4 = #{a=>2, b=>#{hello=>1, home=>2, jeez=>#{c=>6, d=>7}}, e=>4},
        ?assertEqual(Map4, decode(encode(Map4))).

-endif.