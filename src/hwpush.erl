%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-module(hwpush).

-include("hwpush.hrl").

-define(MAX_TOPIC_LEN, 5).

%% API

-export([connect/1]).

-export([disconnect/1]).

%% 推送单条消息
-export([push_to_regid/4]).


%% Util
-export([milliseconds_utc_since_1970/1]).

-type url() :: nonempty_string().

-type year()     ::2000..10000.
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 1..24.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type milliseconds() :: non_neg_integer().
-type date()     :: {year(), month(), day()}.
-export_type([hour/0, minute/0, second/0, milliseconds/0, date/0]).

-type registration_id() :: binary()| string().
-type alias() :: string()| binary().
-type account() :: string()| binary().
-export_type([account/0, alias/0]).

-export_type([registration_id/0]).

-type push_msg() :: android_push_msg().
-export_type([push_msg/0]).

-type android_push_msg()  ::
#{	messages => nonempty_string(), %%消息的内容.（注意：需要对payload字符串做urlencode处理）
	pass_through => 0 | 1, %%0 表示通知栏消息 1 表示透传消息
	title => string(), %%通知栏展示的通知的标题
	description => string() %%通知栏展示的通知的描述
}.

-type connection() ::
#{	host => nonempty_string(),
	port => pos_integer(),
	ssl_opts => list(),
	timeout =>  pos_integer(),
	expires => pos_integer(),
	expires_conn => pos_integer(),
	socket => any(),
	err_callback => fun((binary()) -> stop | _)
}.
-export_type([connection/0]).

%% <<"code">>|<<"data">>|<<"description">>|<<"info">>|<<"result">>|<<"trace_id">>
-type result() :: #{binary() => any()}.
-export_type([result/0]).

-spec connect(connection()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}.
connect(Connection = #{})  ->
  hwpush_sup:start_connection(merge_connection(Connection)).

-spec disconnect(pid()) -> ok|result().
disconnect(ConnId) -> hwpush_connection:stop(ConnId).

%% ===================================================================
%%推送单条消息
%% ===================================================================

%% @doc 向某个regid或一组regid列表推送某条消息
-spec push_to_regid(pid(), list(), push_msg(), return|no_return)-> ok|result().
push_to_regid(ConnID, Token, PushMsg, ReturnType) ->
  NewPushMsg = case maps:get(pass_through, PushMsg, undefined) of
        1 ->
			maps:merge(?SINGLE_ARGS#{<<"deviceToken">> => Token}, maps:remove(pass_through, PushMsg));
        0 ->
         #{title := Title, description := Content} = PushMsg,
         AndroidMsg = jiffy:encode(#{<<"notification_title">> => Title, <<"notification_content">> => Content, <<"doings">> => 1}),
         maps:merge(?NOTIFICATION_ARGS#{tokens => Token, <<"android">> => AndroidMsg}, maps:without([pass_through, title, description], PushMsg))
  end,  
  Result = hwpush_connection:send_message(ConnID, NewPushMsg, ReturnType),
  simplify_to_result(Result).

%% @doc 自1970年来的UTC毫秒数(国际时间:不是local_time:local_time中国区比universal_time快8小时)
-spec milliseconds_utc_since_1970({{year(), month(), day()}, {hour(), minute(), second()}}) -> milliseconds().
milliseconds_utc_since_1970({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Time) ->
  [UTCTime] = calendar:local_time_to_universal_time_dst(Time),
  (calendar:datetime_to_gregorian_seconds(UTCTime) -
    calendar:datetime_to_gregorian_seconds({{1970, 01, 01}, {0, 0, 0 }})) * 1000.

%% ===================================================================
%% INTERNAL FUNCTION
%% ===================================================================

merge_connection(Connection) ->
  Default = #{host => "api.vmall.com",
    name => undefined,
    port => 443,
    appid => "10679334",
    app_secret => "a8aafcd266d477ba0c03cab9210f5e30",
    ssl_opts => [{nodelay, true}, {reuseaddr, true}],
    timeout =>  30000, %% ms
    expires => 300, %% s
    expires_conn => 0,
    socket => undefined,
    err_callback => fun(T) -> io:format("~p~n", [T]) end
  },
  maps:merge(Default, Connection).

%% EXCEPT 差集 INTERSECTION 交集 UNION 并集
check_topic(Topics, OP)when OP == "UNION" orelse OP == "INTERSECTION" orelse OP == "EXCEPT" ->
  case  erlang:length(Topics) > ?MAX_TOPIC_LEN of
    true -> {error, {"topic should =<", ?MAX_TOPIC_LEN}};
    false -> ok
  end;
check_topic(Topic, OP) -> {error, {"topic operation can't be", {Topic, OP}}}.

join([ID| _RestIDs] = IDs, Sep) when is_binary(ID) ->
  join([binary:bin_to_list(IDtmp) ||IDtmp <- IDs], Sep);
join([ID| RestIDs], Sep) ->
  ID ++ lists:append([Sep ++ X || X <- RestIDs]).

transform_extra([]) -> [];
transform_extra([{Target, Message}|RestMsgs]) ->
  [#{target => list_to_binary(Target), message => transform_message(Message)}|transform_extra(RestMsgs)].

transform_message(Message) ->
  NewMessage = maps:without(?EXTRA_LIST, Message),
  ExtraList =
    lists:foldl(fun(Key, Acc) ->
      case maps:get(Key, Message, undefined) of
        undefined -> Acc;
        Value -> maps:put(list_to_binary(atom_to_list(Key) -- "extra."), to_binary(Value), Acc)
      end end, #{}, ?EXTRA_LIST),
  NewMessage#{extra => ExtraList}.

simplify_to_result(ok) -> ok;
simplify_to_result([First|Rest]) ->
  jsx:decode(lists:foldl(fun(B, Acc) -> <<Acc/binary, ", ", B/binary>> end, First, Rest), [return_maps]);
simplify_to_result(Err) -> ok.

format_date({Year, Month, Day})->
  MonthStr =
    case Month < 10 of
      false -> erlang:integer_to_list(Month);
      true -> "0" ++ erlang:integer_to_list(Month)
    end,
  DayStr =
    case Day < 10 of
      true -> "0" ++ erlang:integer_to_list(Day);
      false -> erlang:integer_to_list(Day)
    end,
  erlang:integer_to_list(Year) ++ MonthStr ++ DayStr.

to_binary(Value)when is_list(Value) -> list_to_binary(Value);
to_binary(Value)when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value)when is_atom(Value) -> atom_to_binary(Value, latin1);
to_binary(Value)when is_binary(Value) -> Value.
