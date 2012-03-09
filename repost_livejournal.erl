%%%=============================================================================
%%% @doc Repost an entry to Livejournal.com
%%%
%%% @author Dmitrii 'Mamut' Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2012 Dmitrii 'Mamut' Dimandt
%%%
%%% @end
%%%=============================================================================

-module(repost_livejournal).

-export([repost_function/2]).

repost_function(Config, _Context) ->
  Username = proplists:get_value(username, Config),
  Password = proplists:get_value(password, Config),
  Journal  = proplists:get_value(journal,  Config),
  Fun = fun(EntryId, Ctxt) -> 
          repost(Username, Password, Journal, EntryId, Ctxt)
        end,
  Fun.

%%
%% @doc Livejournal uses XML-RPC. We could construct the request ourselves
%%      Only... Why would we want to do that?
%%      Requires https://github.com/richcarl/xmlrpc/ 
%%
repost(Username, Password, Journal, EntryId, Context) ->
  %% Repost the whole body for now
  Body0 = m_rsc:p(EntryId, body, Context),
  Body  = get_text(Body0, Context),
  
  Title0 = m_rsc:p(EntryId, title, Context),
  Title  = get_text(Title0, Context),
  
  Url   = get_url(EntryId, Context),
  {{Year, Month, Day},{Hour, Minute, _}} = 
          calendar:now_to_universal_time(now()),
  Body1 = Body ++ Url,
  xmlrpc:call( "www.livejournal.com"
             , 80
             , "/interface/xmlrpc"
             , { call
               , 'LJ.XMLRPC.postevent'
               , [{struct, [ {username, Username}
                           , {password, Password}
                           , {event, Body1}
                           , {subject, Title}
                           , {year, Year}, {mon, Month}, {day, Day}
                           , {hour, Hour}, {min, Minute}
                           , {usejournal, Journal}]}]}).

get_url(EntryId, Context) ->
  Url = 
    z_context:abs_url(z_convert:to_binary(m_rsc:p(EntryId, page_url, Context))
                     , Context),
  lists:flatten([ "\n\n", "<a href=\"", Url, "\">", Url, "</a>"]).

get_text({trans, List}, Context) ->
  Language = z_context:language(Context),
  Text = proplists:get_value(Language, List, proplists:get_value(en, List, "")),
  TextWithMedia = filter_show_media:show_media(Text, Context),
  AbsUrl = z_context:abs_url("/", Context),
  TextAsList = process_text(lists:flatten(z_convert:to_list(TextWithMedia))),
  process_links(TextAsList, AbsUrl).

process_text(Text) ->
  lists:flatten(process_text(Text, [])).

  process_text([], Acc) ->
    Acc;
  process_text([H|T], Acc) when is_binary(H) ->
    process_text(T, [Acc | binary_to_list(H)]);
  process_text([H|T], Acc) ->
    process_text(T, [Acc | [H]]).

process_links(Text, Url) ->
  process_links(Text, Url, []).

  process_links([], _, Acc) ->
    lists:flatten(Acc);
  % process src="/
  process_links([$s, $r, $c, $=, Q, $/ | T], Url, Acc) when   Q =:= $\"
                                                            ; Q =:= $\'->
    process_links(T, Url, [Acc | ["src=", Q, Url]]);
  % process href="/
  process_links([$h, $r, $e, $f, $=, Q, $/ | T], Url, Acc)  when   Q =:= $\"
                                                                 ; Q =:= $\'->
    process_links(T, Url, [Acc | ["href=", Q, Url]]);
  process_links([H|T], Url, Acc) ->
    process_links(T, Url, [Acc | [H]]).
