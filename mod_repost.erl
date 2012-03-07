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

%%
%% @doc When resource update is done, repost it
%%
observe_rsc_update_done( {_Action, EntryId, _Pre, _Post, _PreProps, _PostProps}
                       , Context) ->
  case can_repost(EntryId, Context) of
    true  -> repost(EntryId, Context);
    false -> ok
  end.

%%
%% @doc Repost only if it's published and is not a category
%%
can_repost(EntryId, Context) ->
  m_rsc:is_visible(EntryId, Context) and m_rsc:get(EntryId, Context) =:= [].

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
  RepostModules = m_config:get(?MODULE, Context),
  get_repost_functions(RepostModules, Context, []).

get_repost_functions([], _Context, Acc) ->
  Acc;
get_repost_functions([Module|T], Context, Acc) ->
  Config = m_config:get(?MODULE, Module, Context),
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
