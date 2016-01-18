-module(cuttlefish_parse).

-type variable()     :: [string()].
-type conf()         :: [{variable(), term()}].
-type parse_result() :: {error, term()} 
                      | {conf(), binary(), {{line, integer()}, {column, integer()}}}
                      | conf().

-callback file(iolist()) -> parse_result().