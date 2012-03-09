%%%=============================================================================
%%% @doc Repost an entry to Twitter
%%%
%%% @author Dmitrii 'Mamut' Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2012 Dmitrii 'Mamut' Dimandt
%%%
%%% @end
%%%=============================================================================

-module(repost_twitter).

-export([repost_function/2]).

-define(CONTENT_TYPE, "application/x-www-form-urlencoded").
-define(TWITTER_URL,  
        "http://api.twitter.com/1/statuses/update.json?include_entities=true").

%%
%% @doc config should contain the following fields:
%%        oauth_consumer_key
%%        oauth_token
%%      You can obtain these here: 
%%        https://dev.twitter.com/docs/auth/obtaining-access-tokens
%%      The easiest way would be to crate an app for that, see
%%        https://dev.twitter.com/apps
%%      Auth is described in detail here:
%%        https://dev.twitter.com/docs/auth/authorizing-request
%%

repost_function(Config, _Context) ->
  Fun = fun(EntryId, Ctxt) -> 
          repost(Config, EntryId, Ctxt)
        end,
  Fun.

%%
%% @doc https://dev.twitter.com/docs/auth/authorizing-request
%%      https://dev.twitter.com/docs/api/1/post/statuses/update
%%
%%      For now, we let Twitter shorten links itself (20-25 characters)
%%      We only deal with titles and summaries, since this way we don't
%%      have to clean up links and media from entry body
%%
repost(Config, EntryId, Context) ->
  ConsumerKey    = proplists:get_value(oauth_consumer_key, Config),
  ConsumerSecret = proplists:get_value(oauth_consumer_secret, Config),
  Token          = proplists:get_value(oauth_token, Config),
  TokenSecret    = proplists:get_value(oauth_token_secret, Config),

  Body0 = m_rsc:p(EntryId, summary, Context),
  Body  = get_text(Body0, Context),
  
  Title0 = m_rsc:p(EntryId, title, Context),
  Title  = get_text(Title0, Context),

  Url   = get_url(EntryId, Context),
  
  PostBody = lists:flatten(get_post_body(Title, Body) ++ Url),
  
  Nonce = generate_nonce(EntryId),
  SignatureMethod = "HMAC-SHA1",
  TimeStamp = z_convert:to_list(
                z_datetime:datetime_to_timestamp(
                  calendar:local_time()) + 3600),
  Version = "1.0",
  RequestParams = prepare_params([ {"status", PostBody}
                                 , {"include_entities", "true"}
                                 , {"oauth_consumer_key", ConsumerKey}
                                 , {"oauth_nonce", Nonce}
                                 , {"oauth_signature_method", SignatureMethod}
                                 , {"oauth_timestamp", TimeStamp}
                                 , {"oauth_token", Token}
                                 , {"oauth_version", Version}]),
  Signature = generate_signature(RequestParams, ConsumerSecret, TokenSecret),
  AuthHeader = 
    generate_auth_header( ConsumerKey, Token, Nonce, SignatureMethod, Signature
                        , TimeStamp, Version),
  
  Payload = list_to_binary(["status=", z_utils:url_encode(PostBody)]),
  Res = httpc:request( post                                 % method()
               , { ?TWITTER_URL                       % request() = { url
                    , [{"Authorization", AuthHeader}] %             , headers
                    , ?CONTENT_TYPE                   %             , content-type
                    , Payload}                        %             , body }
                , []                                  % http_options()
                , []),                                % options()
  io:format("~p", [Res]).


get_post_body(Title, Body) ->
  z_string:truncate([Title | Body], 114).  %% 140 - 25 for link - 1 for ... char 

generate_nonce(EntryId) ->
  {{Year, Month, Day},{Hour, Minute, Sec}} = 
          calendar:now_to_universal_time(now()),
  Nonce = io_lib:format( "~p~p~p~p~p~p~p"
                       , [Year, Month, Day, Hour, Minute, Sec, EntryId]),
  lists:flatten(Nonce).

generate_signature(Params, ConsumerKey, Token) ->
  SignatureString = lists:flatten([ "POST&"
                                  , z_utils:url_encode("statuses/update")
                                  , "&"
                                  , z_utils:url_encode(Params)]),
  SigningKey = lists:flatten([ z_utils:url_encode(ConsumerKey)
                             , "&"
                             , z_utils:url_encode(Token)
                             ]),
  Sha = crypto:sha_mac(SigningKey, SignatureString),
  z_convert:to_list(base64:encode(Sha)).

prepare_params(Params) ->
  prepare_params(Params, []).

prepare_params([], Acc) ->
  Ordlist = orddict:from_list(Acc),
  params_to_string(Ordlist);
prepare_params([{Key, Value} | T], Acc) ->
  Acc1 = [{z_utils:url_encode(Key), z_utils:url_encode(Value)} | Acc],
  prepare_params(T, Acc1).

params_to_string(Ordlist) ->
  KeyValueList =
      [lists:flatten([Key, "=", Value]) || {Key, Value} <- Ordlist],
  string:join(KeyValueList, "&").

generate_auth_header( ConsumerKey, Token, Nonce, SignatureMethod, Signature
                        , TimeStamp, Version) ->
  lists:flatten([ "OAuth "
                , "oauth_consumer_key=\"", ConsumerKey, "\","
                , "oauth_nonce\"", Nonce, "\","
                , "oauth_signature=\"", Signature, "\","
                , "oauth_signature_method=\"", SignatureMethod, "\","
                , "oauth_timestamp=\"", TimeStamp, "\","
                , "oauth_token=\"", Token, "\","
                , "oauth_version=\"", Version, "\""]).

get_text({trans, List}, Context) ->
  Language = z_context:language(Context),
  Text = proplists:get_value(Language, List, proplists:get_value(en, List, "")),
  TextWithMedia = filter_show_media:show_media(Text, Context),
  process_text(lists:flatten(z_convert:to_list(TextWithMedia))).

process_text(Text) ->
  lists:flatten(process_text(Text, [])).

  process_text([], Acc) ->
    Acc;
  process_text([H|T], Acc) when is_binary(H) ->
    process_text(T, [Acc | binary_to_list(H)]);
  process_text([H|T], Acc) ->
    process_text(T, [Acc | [H]]).

get_url(EntryId, Context) ->
  z_context:abs_url( z_convert:to_binary(m_rsc:p(EntryId, page_url, Context))
                   , Context).
