-module(tlv).

-export([encode/1, decode/1]).

encode(Num) when is_integer(Num) ->
    Type = 0,
    Bin = integer_to_binary(Num, 10),
    Len = bit_size(Bin),
    <<Type:4, Len:16, Bin/bitstring>>;
encode(Atom) when is_atom(Atom) ->
    Type = 1,
    Bin = atom_to_binary(Atom, latin1),
    Len = bit_size(Bin),
    <<Type:4, Len:16, Bin/bitstring>>;
encode(Binary) when is_binary(Binary) ->
    Type = 2,
    Len = bit_size(Binary),
    <<Type:4, Len:16, Binary/bitstring>>;
encode(Map) when is_map(Map) ->
    Type = 3,
    Bin = encode_map(maps:to_list(Map), <<>>),
    Len = bit_size(Bin),
    <<Type:4, Len:16, Bin/bitstring>>.


encode_map([{X,Y}|T], Result) ->
    Bin1 = encode(X),
    Bin2 = encode(Y),
    NewBinary = <<Result/bitstring, Bin1/bitstring, Bin2/bitstring>>,
    encode_map(T, NewBinary);
encode_map([], Result) ->
    Result.

decode(Input) ->
    {Answer, <<>>} = decode_helper(Input),
    Answer.

decode_helper(<<0:4, Len:16, Rest/bitstring>>) ->
    <<Num:Len/bitstring, NewRest/bitstring>> = Rest, 
    {binary_to_integer(Num), NewRest};
decode_helper(<<1:4, Len:16, Rest/bitstring>>) -> 
    <<Atom:Len/bitstring, NewRest/bitstring>> = Rest,
    {binary_to_atom(Atom, latin1), NewRest};
decode_helper(<<2:4, Len:16, Rest/bitstring>>) -> 
    <<Binary:Len/bitstring, NewRest/bitstring>> = Rest,
    {Binary, NewRest};
decode_helper(<<3:4, Len:16, Rest/bitstring>>) -> 
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