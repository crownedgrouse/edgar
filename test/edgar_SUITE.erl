-module(edgar_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([extract/1, extract_long/1, extract_mixte1/1, create/1, create_long/1, create_mixte1/1]).
%-export([ add/1, add_long/1, add_mixte1/1]). % TODO

-define(value(Key,Config), proplists:get_value(Key,Config)).

%%% reference files was created with : GNU ar (GNU Binutils for Ubuntu) 2.23.2


-define(extract(File),
    Config = Config_ ++ [{ar_ref, File}],
    F = ?value(ar_ref, Config),
    D = ?value(data_dir, Config),
    Ref = filename:join(D, F),
    ok = edgar:extract(Ref, [{cwd, ?value(priv_dir, Config)}])
 ).

-define(create(File),
    C = Config_ ++ [{ar_ref, File}],
    F = ?value(ar_ref, C),
    D = ?value(data_dir, C) ,
    Ref = filename:join(D, F),
    {ok, List } = edgar:table(Ref),
    ct:pal("Ar File list : ~p",[List]),
    Config = C ++ [{files,List}],
    ct:pal("Ar reference : ~s",[Ref]),
    % Creating ar archive
    T = ?value(priv_dir, Config) ,
    Test = filename:join(T, F),
    ct:pal("Ar created   : ~s",[Test]),
    {ok, OrigCwd} = file:get_cwd(),
    file:set_cwd(T),
    List = ?value(files, Config) ,
    ct:pal("List : ~p",[List]),
    edgar:create(Test, List),
    file:set_cwd(OrigCwd),
    % Compare files
    A = file:read_file(Ref),
    B = file:read_file(Test),
    file:delete(Test),
    A = B
  ).

-define(add(File),
    {ok, List } = edgar:table(Ref),
    ct:pal("Ar File list : ~p",[List]),
    % Creating ar archive
    T = ?value(priv_dir, Config) ,
    Test = filename:join(T, F),
    TestAd = edgar:open(Test, [write]),
    ct:pal("Ar descriptor (add) : ~p",[TestAd]),
    {ok, OrigCwd} = file:get_cwd(),
    file:set_cwd(T),
    %List = ?value(files, Config) ,
    % Add files
    ok = lists:foreach(fun(Filename) -> ct:pal("~p",[Filename]) end, List),
    ok = lists:foreach(fun(Filename) -> ct:pal("~p",[Filename]), ct:pal("~p", [edgar:add(TestAd, Filename, [write])]) end, List),
    edgar:close(TestAd),
    file:set_cwd(OrigCwd),
    % Compare files
    A = file:read_file(Ref),
    B = file:read_file(Test),
    file:delete(Test),
    A = B
  ).

%suite() ->
%    [{timetrap,{minutes,10}}].

%init_per_suite(Config) ->
%    Config.

%end_per_suite(_Config) ->
%    ok.

%init_per_group(_GroupName, Config) ->
%    Config.

%end_per_group(_GroupName, _Config) ->
%    ok.

%init_per_testcase(_TestCase, Config) ->
%    Config.

%end_per_testcase(_TestCase, _Config) ->
%    ok.

%groups() ->
%    [].

all() ->
    [extract, extract_long, extract_mixte1, create, create_long, create_mixte1]. % TODO add, add_long, add_mixte1

extract(Config_) -> ?extract("ar_ref.ar").

extract_long(Config_) -> ?extract("ar_ref_long.ar").

extract_mixte1(Config_) -> ?extract("ar_ref_mixte1.ar").


create(Config_) -> ?create("ar_ref.ar").

create_long(Config_) -> ?create("ar_ref_long.ar").

create_mixte1(Config_) -> ?create("ar_ref_mixte1.ar").


%add(Config_) -> ?extract("ar_ref.ar"),?add("ar_ref.ar").

%add_long(Config_) -> ?extract("ar_ref_long.ar"),?add("ar_ref_long.ar").

%add_mixte1(Config_) -> ?extract("ar_ref_mixte1.ar"),?add("ar_ref_mixte1.ar").

