%%%=============================================================================
%%% @doc 
%%%
%%% @author Dmitrii 'Mamut' Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2012 Dmitrii 'Mamut' Dimandt
%%%
%%% @end
%%%=============================================================================

-module(mod_repost).

-author("Dmitrii 'Mamut' Dimandt <dmitrii@dmitriid.com>").
-mod_title("Repost module").
-mod_description("Repost site entries to other sites").
-mod_prio(900).

-export([observe_rsc_update_done/2]).

-include_lib("./../include/zotonic_notifications.hrl").

%%
%% @doc When resource update is done, repost it
%%
observe_rsc_update_done(#rsc_update_done{id=EntryId}, Context) ->
  case can_repost(EntryId, Context) of
    true  -> repost(EntryId, Context);
    false -> ok
  end.

%%
%% @doc Repost only if it's published and is not a category
%%
can_repost(EntryId, Context) ->
       m_rsc:is_visible(EntryId, Context) 
  and (m_category:get(EntryId, Context) =:= undefined).

%%
%% @doc Retrieve the actual entry, gather all reposting functions, fire them
%%
repost(EntryId, Context) ->
  RepostFunctions = get_repost_functions(Context),
  do_repost(RepostFunctions, EntryId, Context).

%%
%% @doc Retrieve a list of repost modules and their configurations
%%
get_repost_functions(Context) ->
  RepostModules = z_config:get(?MODULE, Context),
  get_repost_functions(RepostModules, Context, []).

get_repost_functions([], _Context, Acc) ->
  Acc;
get_repost_functions([{Module, Config}|T], Context, Acc) ->
  Fun = Module:repost_function(Config, Context),
  get_repost_functions(T, Context, [Fun | Acc]).

%%
%% @doc Fire each repos function in succession
%%
do_repost([], _Entry, _Context) ->
  ok;
do_repost([Fun|T], EntryId, Context) ->
  Fun(EntryId, Context),
  do_repost(T, EntryId, Context).
