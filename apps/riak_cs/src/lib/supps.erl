%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2022, 2023 TI Tokyo    All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------


-module(supps).

-export([q/1, p/1, p/0]).

-ignore_xref([q/1, p/1, p/0]).

-record(p, {name :: atom() | tuple(),
            info :: undefined | proplists:proplist(),
            total_mem = 0 :: non_neg_integer(),
            type :: worker | supervisor,
            pid :: pid() | undefined,
            children = []
           }
       ).

usage() ->
    "A ps-like output of process_info items (memory, message_queue_len\n"
    "etc) of processes under the supervisor trees of riak_cs_sup.\n"
    "\n"
    "Options:\n"
    "  --format flat|tree, selects the output format (default is 'tree');\n"
    "  --depth Depth, print children up to Depth level deep (default\n"
    "      is 2, meaning top-level sups with their immediate children). Depth\n"
    "      can be 'max';\n"
    "  --filter Regex, filter on process names to use (default is \".+\"):\n"
    "    * if a sup's name matches, all its children are shown;\n"
    "    * if a sup's name doesn't match, it is only shown if a match is\n"
    "      found in its children or below;\n"
    "  --order_by, ProcessInfoItem, sort by this process_info item, or 'none'\n"
    "      to preserve the order children are created (default is 'memory').\n".


sups() ->
    [riak_cs_sup].


-spec p(proplists:proplist()) -> ok.
p(Opts) ->
    io:put_chars(q(Opts)).

-spec p() -> ok.
p() ->
    io:put_chars(q([])).

-spec q(string() | proplists:proplist()) -> string().
q(["--help"]) ->
    usage();
q([O|_] = Options) when not is_tuple(O) ->
    q(validate_options(Options, []));
q(Options0) ->
    Nodes = [node() | nodes()],
    Options = extract_options(Options0),

    erlang:put(supps_output, []),
    lists:foreach(
      fun(Node) ->
              acc(io_lib:format("============ Node: ~s ===========================\n", [Node])),
              acc(io_lib:format("~11s\t~5s\t~8s\t~.14s~s\n"
                                "-------------------------------------------------------------\n",
                                [mem, mq, ths, pid, process])),
              print(
                reformat([get_info(Node, P) || P <- sups()], Options),
                Options, 0)
      end, Nodes
     ),
    lists:flatten(lists:reverse(erlang:get(supps_output))).

validate_options([], Good) ->
    Good;
validate_options(["--depth", "max" | R], Q) ->
    validate_options(R, [{depth, max} | Q]);
validate_options(["--depth", A | R], Q) ->
    validate_options(R, [{depth, list_to_integer(A)} | Q]);
validate_options(["--format", "flat" | R], Q) ->
    validate_options(R, [{format, flat} | Q]);
validate_options(["--format", "tree" | R], Q) ->
    validate_options(R, [{format, tree} | Q]);
validate_options(["--filter", A | R], Q) ->
    validate_options(R, [{filter, A} | Q]);
validate_options(["--order_by", A | R], Q) when A == "none";
                                                A == "memory" ->
    validate_options(R, [{order_by, list_to_atom(A)} | Q]);
validate_options([K], Q) ->
    io:format("Option requires a value: ~s\n", [K]),
    validate_options([], Q);
validate_options([K, V | R], Q) ->
    io:format("Invalid option ~s or value ~s\n", [K, V]),
    validate_options(R, Q).

extract_options(PL) ->
    Depth =
        case proplists:get_value(depth, PL, 1) of
            max ->
                9999;
            V ->
                V
        end,
    #{filter => proplists:get_value(filter, PL, ".+"),
      format => proplists:get_value(format, PL, tree),
      order_by => proplists:get_value(order_by, PL, memory),
      depth => Depth
     }.


get_info(Node, Name) ->
    FF =
        lists:foldl(
          fun({SubName, Pid, worker, _MM}, Q) ->
                  Info = rpc:call(Node, erlang, process_info,
                                  [Pid, [memory, message_queue_len, messages, total_heap_size]]),
                  [#p{name = SubName, info = Info, type = worker, pid = Pid,
                      total_mem = proplists:get_value(memory, Info, 0)} | Q];
             ({SubName, _Pid, supervisor, _MM}, Q) ->
                  [get_info(Node, SubName) | Q]
          end,
          [],
          case rpc:call(Node, supervisor, which_children, [Name]) of
              Children when is_list(Children) ->
                  Children;
              _ ->
                  []
          end
         ),
    #p{name = Name,
       total_mem = lists:foldl(fun(#p{total_mem = TM}, Q) ->
                                       Q + TM
                               end, 0, FF),
       type = supervisor,
       children = FF}.


print(_p, #{depth := MaxDepth}, Depth) when MaxDepth < Depth ->
    ok;
print(PP, Options, Depth) when is_list(PP) ->
    lists:foreach(fun(P) -> print(P, Options, Depth) end, PP);
print(#p{name = Name, info = Info, type = worker, pid = Pid},
      #{filter := Filter}, Depth) ->
    case re:run(printable(Name), Filter) of
        nomatch ->
            skip;
        _ ->
            Mem = integer_or_blank(memory, Info),
            THS = integer_or_blank(total_heap_size, Info),
            MQ = integer_or_blank(message_queue_len, Info),
            acc(io_lib:format("~11s\t~5s\t~8s\t~.14s~s~s\n", [Mem, MQ, THS, pid_to_list(Pid), pad(Depth * 2), printable(Name)]))
    end;
print(#p{name = Name, children = FF, total_mem = Mem} = P,
      Options = #{filter := Filter}, Depth) ->
    case has_printable_children(P, Filter) of
        no ->
            skip;
        Yes ->
            acc(io_lib:format("~11b\t~5s\t~8s\t~14s~s~s (~b)\n",
                              [Mem, "", "", "", pad(Depth * 2), printable(Name), length(FF)])),
            lists:foreach(
              fun(F) -> print(F, Options#{filter => maybe_drop_filter(Yes, Filter)}, Depth + 1) end,
              FF
             )
    end.
maybe_drop_filter(all, _) -> ".+";
maybe_drop_filter(yes, Filter) -> Filter.


integer_or_blank(F, Info) ->
    A_ = proplists:get_value(F, Info, ""),
    [integer_to_list(A_)||is_integer(A_)].

pad(N) ->
    lists:duplicate(N, $ ).

printable(Name) when is_atom(Name) -> atom_to_list(Name);
printable(Name) -> io_lib:format("~p", [Name]).


has_printable_children(#p{name = Name, type = worker}, Filter) ->
    case re:run(printable(Name), Filter) of
        nomatch ->
            no;
        _ ->
            yes
    end;
has_printable_children(#p{name = Name, children = FF}, Filter) ->
    case re:run(printable(Name), Filter) of
        nomatch ->
            case lists:any(
                   fun(P) -> no /= has_printable_children(P, Filter) end,
                   FF) of
                true ->
                    yes;
                false ->
                    no
            end;
        _ ->
            all
    end.

reformat(PP, #{format := tree, order_by := none}) ->
    PP;
reformat(PP, #{format := tree, order_by := memory} = Options) ->
    lists:map(
      fun(P = #p{children = []}) ->
              P;
         (P = #p{children = FF}) ->
              P#p{children = lists:sort(
                               fun(#p{total_mem = M1}, #p{total_mem = M2}) ->
                                       M1 > M2
                               end,
                               reformat(FF, Options))}
      end,
      PP);
reformat(#p{children = PP}, Options) ->
    reformat(PP, Options);
reformat(PP, #{format := flat, order_by := OrderBy} = Options) ->
    PP1 =
        lists:flatten(
          lists:foldl(
            fun(#p{type = worker} = P, Q) ->
                    [P | Q];
               (#p{type = supervisor, children = FF}, Q) ->
                    lists:concat([reformat(FF, Options), Q])
            end,
            [],
            PP
           )
         ),
    case OrderBy of
        none ->
            PP1;
        memory ->
            lists:sort(fun(#p{total_mem = M1}, #p{total_mem = M2}) ->
                               M1 > M2
                       end, PP1)
    end.

acc(L) ->
    Q = erlang:get(supps_output),
    erlang:put(supps_output, [L|Q]).
