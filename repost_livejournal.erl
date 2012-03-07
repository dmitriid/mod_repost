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
  Body  = m_rsc:p(EntryId, body, Context),
  Title = m_rsc:p(EntryId, title, Context),
  Url   = get_url(m_rsc:p(EntryId, page_url, Context)),
  {{Year, Month, Day},{Hour, Minute, _}} = 
          calendar:now_to_universal_time(now()),
  Body1 = [Body | Url],
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


get_url(Url) ->
  lists:flatten("\n\n", "<a href=\"", Url, "\">", Url, "</a>").