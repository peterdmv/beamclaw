-module(bc_webhook_telegram_h_tests).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% verify_token/2 — valid secret
%%--------------------------------------------------------------------

valid_secret_test() ->
    ?assertEqual(ok, bc_webhook_telegram_h:verify_token(<<"my-secret">>, <<"my-secret">>)).

valid_secret_long_test() ->
    Token = <<"a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6">>,
    ?assertEqual(ok, bc_webhook_telegram_h:verify_token(Token, Token)).

%%--------------------------------------------------------------------
%% verify_token/2 — wrong secret
%%--------------------------------------------------------------------

wrong_secret_test() ->
    ?assertEqual({error, <<"invalid token">>},
                 bc_webhook_telegram_h:verify_token(<<"wrong">>, <<"right">>)).

wrong_secret_same_length_test() ->
    ?assertEqual({error, <<"invalid token">>},
                 bc_webhook_telegram_h:verify_token(<<"aaaa">>, <<"bbbb">>)).

%%--------------------------------------------------------------------
%% verify_token/2 — missing header
%%--------------------------------------------------------------------

missing_header_test() ->
    ?assertEqual({error, <<"missing header">>},
                 bc_webhook_telegram_h:verify_token(undefined, <<"some-secret">>)).

missing_header_no_config_test() ->
    ?assertEqual({error, <<"missing header">>},
                 bc_webhook_telegram_h:verify_token(undefined, undefined)).

%%--------------------------------------------------------------------
%% verify_token/2 — no secret configured (fail-closed)
%%--------------------------------------------------------------------

no_secret_configured_test() ->
    ?assertEqual({error, <<"no secret configured">>},
                 bc_webhook_telegram_h:verify_token(<<"provided">>, undefined)).

%%--------------------------------------------------------------------
%% verify_token/2 — different lengths (constant-time safe)
%%--------------------------------------------------------------------

different_length_short_vs_long_test() ->
    ?assertEqual({error, <<"invalid token">>},
                 bc_webhook_telegram_h:verify_token(<<"short">>, <<"a-much-longer-secret">>)).

different_length_long_vs_short_test() ->
    ?assertEqual({error, <<"invalid token">>},
                 bc_webhook_telegram_h:verify_token(<<"a-much-longer-secret">>, <<"short">>)).

%%--------------------------------------------------------------------
%% verify_token/2 — empty strings
%%--------------------------------------------------------------------

both_empty_test() ->
    ?assertEqual(ok, bc_webhook_telegram_h:verify_token(<<>>, <<>>)).

one_empty_test() ->
    ?assertEqual({error, <<"invalid token">>},
                 bc_webhook_telegram_h:verify_token(<<>>, <<"notempty">>)).
