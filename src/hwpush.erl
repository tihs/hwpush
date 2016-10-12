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

-type push_msg() :: android_push_msg() | ios_push_msg().
-export_type([push_msg/0]).

-type android_push_msg()  ::
#{payload => nonempty_string(), %%消息的内容.（注意：需要对payload字符串做urlencode处理）
regestricted_package_name => string(), %%App的包名, packageName必须和开发者网站上申请的结果一致
pass_through => 0 | 1, %%0 表示通知栏消息 1 表示透传消息
title => string(), %%通知栏展示的通知的标题
description => string(), %%通知栏展示的通知的描述
notify_type => string(), %%可以是DEFAULT_ALL或者以下其他几种的OR组合

%% 以上为必填项, 以下为可选项
%%可选项, 如果用户离线, 设置消息在服务器保存的时间, 单位:ms. 服务器默认最长保留两周:1209600000
time_to_live => 1..1209600000,
%%可选项, 定时发送消息. 仅支持七天内的定时消息, 用自1970年1月1日以来00:00:00.0 UTC时间表示（毫秒）
time_to_send => pos_integer(),
%%可选项, 默认情况下, 通知栏只显示一条推送消息.
%%如果通知栏要显示多条推送消息, 需要针对不同的消息设置不同的notify_id
%%相同notify_id的通知栏消息会覆盖之前的
notify_id => pos_integer(),
%%可选项, 自定义通知栏消息铃声. extra.sound_uri的值设置为铃声的URI, 不要加铃声文件的后缀,
%%如:"android.resource://com.xiaomi.hwpushdemo/raw/shaking"铃声只能使用当前app内的资源,
%%URI格式满足 android.resource://your packagename/XXX/XXX, 铃声文件放在Android app的raw目录下
'extra.sound_uri' => nonempty_string(),
%%可选项, 开启通知消息在状态栏滚动显示, 如: "我是滚动的文字"
'extra.ticker' => string(),
%%可选项, 开启/关闭app在前台时的通知弹出.
%%当extra.notify_foreground值为”1″时, app会弹出通知栏消息;
%%当extra.notify_foreground值为”0″时, app不会弹出通知栏消息。注: 默认情况下会弹出通知栏消息
'extra.notify_foreground' => string(),
%%可选项, 预定义通知栏消息的点击行为. 通过设置extra.notify_effect的值以得到不同的预定义点击行为
%%"1"通知栏点击后打开app的Launcher Activity
%%"2":通知栏点击后打开app的任一Activity（还需要传入extra.intent_uri)
%%"3":通知栏点击后打开网页（还需要传入extra.web_uri）
'extra.notify_effect' => string(),
%% 可选项"intent:#Intent;component=com.yourpackage/.YourActivity;end"
'extra.intent_uri' => string(),
%%可选项 "http://www.google.com"
'extra.web_uri' => url(),
%%  可选项, 控制是否需要进行平缓发送（qps less 1000/second）默认不支持 0 表示不支持平缓发送 1 表示支持平缓发送
'extra.flow_control' => integer(),
%% 可选项, 自定义通知栏样式, 设置为客户端要展示的layout文件名 如"custom_notify"
'extra.layout_name' => nonempty_string(),
%%可选项, 自定义通知栏样式, 必须与layout_name一同使用,
%%指定layout中各控件展示的内容 如"{\"text\": {\"titleText\":\"标题\"}, \"image\": {\"iconImage\": \"icon\"}"
'extra.layout_value' => nonempty_string(),
%%可选项, 使用推送批次（JobKey）功能聚合消息. 客户端会按照jobkey去重,
%%即相同jobkey的消息只展示第一条, 其他的消息会被忽略. 合法的jobkey由数字（[0-9]）
%%大小写字母（[a-zA-Z]), 下划线（_）和中划线（-）组成, 长度不大于8个字符
'extra.jobkey' => nonempty_string(),
%%可选项, 开启消息送达和点击回执. 将extra.callback的值设置为第三方接收回执的http接口
%%小米推送服务器每隔1s将已送达或已点击的消息ID和对应设备的regid或alias
%%通过调用第三方http接口传给开发者.
%%每次调用后, 小米推送服务器会清空这些数据.
%%下次传给开发者将是新一拨数据。注：消息的送达回执只支持单发消息.
'extra.callback' => url(),
%%可选项, 可以接收消息的设备的语言范围, 用逗号分隔, 如:中国大陆用"zh_CN"
'extra.locale' => nonempty_string(),
%%可选项, 无法收到消息的设备的语言范围, 逗号分隔
'extra.locale_not_in' => nonempty_string(),
%%可选项, 对应不同品牌的手机或手机价格范畴
'extra.model' => nonempty_string(),
%%可选项, 无法收到消息的设备的机型范围, 逗号分隔
'extra.model_not_in' => nonempty_string(),
%%可选项, 可以接收消息的app版本号, 用逗号分割,
%%安卓app版本号来源于manifest文件中的”android:versionName”的值.
%%注: 目前支持MiPush_SDK_Client_2_2_12_sdk.jar（及以后）的版本.
'extra.app_version' => nonempty_string(),
%%可选项, 无法接收消息的app版本号, 用逗号分割
'extra.app_version_not_in' => nonempty_string(),
%%可选项, 指定在特定的网络环境下才能接收到消息 目前仅支持指定”wifi”
'extra.connpt' => nonempty_string()
}.

-type ios_push_msg() ::
#{description => nonempty_string(), %%通知栏展示的通知的
%% 以上为必填项, 以下为可选项
%%可选项, 如果用户离线, 设置消息在服务器保存的时间, 单位:ms.服务器默认最长保留两周
time_to_live => non_neg_integer(),
%%可选项, 定时发送消息. 用自1970年1月1日以来00:00:00.0 UTC时间表示(以毫秒为单位的时间).
%%注: 仅支持七天内的定时消息
time_to_send => non_neg_integer(),
%%可选项, 自定义消息铃声. 当值为空时为无声, default为系统默认声音
'extra.sound_url' => string(),
%%可选项.通知角标
'extra.badge' => non_neg_integer(),
%%可选项. iOS8推送消息快速回复类别
'extra.category' => non_neg_integer()
}.

-type connection() ::
#{host => nonempty_string(),
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
  Req = {Token, PushMsg},
  Result = hwpush_connection:send_message(ConnID, Req, ReturnType),
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
