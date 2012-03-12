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
-compile(export_all).

-define(TWITTER_URL,  
        "https://api.twitter.com/1/statuses/update.json").

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
  
  PostBody = get_post_body(Title, Body, Url),
  
  Res = oauth:post( ?TWITTER_URL
            , [{"include_entities", "true"}, {"status", PostBody}]
            , {ConsumerKey, ConsumerSecret, hmac_sha1}
            , Token
            , TokenSecret),
  io:format("~p~n~n",[PostBody]),
  io:format("~p~n~n",[Res]).

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
  lists:flatten(z_context:abs_url( z_convert:to_binary(m_rsc:p(EntryId, page_url, Context))
                   , Context)).

get_post_body(Title, Body, Url) ->
  Text = lists:flatten([Title, ": ", Body]),
  %% 140 - (link) - (1 for '...' char)
  lists:flatten([z_string:truncate(Text, 110), " ", Url]).   

generate_nonce(EntryId) ->
  {{Year, Month, Day},{Hour, Minute, Sec}} = 
          calendar:now_to_universal_time(now()),
  Nonce = io_lib:format( "~p~p~p~p~p~p~p"
                       , [Year, Month, Day, Hour, Minute, Sec, EntryId]),
  lists:flatten(Nonce).