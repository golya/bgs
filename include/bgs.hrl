
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEBUG(Msg, List), io:format(Msg++"~n", List)).
