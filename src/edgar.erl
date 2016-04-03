%%%------------------------------------------------------------------------
%%% File:      edgar.erl
%%% @author    Eric Pailleau <edgar@crownedgrouse.com>
%%% @copyright 2014 crownedgrouse.com
%%% @doc  
%%% Erlang Does Gnu AR
%%% @end  
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 16 February 2014 
%%%-------------------------------------------------------------------------

%%%*************************************************************************
%%%***                              WARNING                              ***
%%%*** § 3.3 of EPL impose to "include a prominent statement that the    ***
%%%*** Modification is derived, directly or indirectly, from Original    ***
%%%*** Code provided by the Initial Developer and including the name of  ***
%%%*** the Initial Developer"                                            ***                            
%%%*************************************************************************
%%%***                             STATEMENT                             ***
%%%*** Some pieces of below code was copied/modified from erl_tar.erl    ***
%%%*** which is subject to the Erlang Public License and its initial     ***
%%%*** developper was Ericsson AB .                                      ***
%%%*************************************************************************

-module(edgar).
-author('Eric Pailleau <edgar@crownedgrouse.com>').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEBUG(X), io:format("~p~n",[X]) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Purpose: GNU ar (archive) utility.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([create/2, create/3, extract/1, extract/2, table/1, table/2,
	 open/2, close/1, t/1, tt/1, format_error/1]).
%-export([add/3, add/4]). % TODO

-include_lib("kernel/include/file.hrl").

-record(add_opts,
	{read_info,				% Fun to use for read file/link info.
	 verbose = false :: boolean()}).	% Verbose on/off.

-define(REM(Size),
	case (Size rem 2) of
	      0 -> Size ;
	      1 -> Size + 1
	end).

-define(REM01(Size),
	case (Size rem 2) of
	      0 -> 0 ;
	      1 -> 1 
	end).
%%-------------------------------------------------------------------------
%% @doc Opens an ar archive.
%% @end
%%-------------------------------------------------------------------------
open(Name, Mode) ->
    case open_mode(Mode) of
	{ok, Access, Raw, Opts} ->
	    open1(Name, Access, Raw, Opts);
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

open1({binary,Bin}, read, _Raw, Opts) ->
    case file:open(Bin, [ram,binary,read]) of
	{ok,File} ->
	    case Opts of
		[compressed] -> ram_file:uncompress(File);
		[] -> ok
	    end,
	    {ok,{read,File}};
	Error ->
	    Error
    end;
open1({file, Fd}, read, _Raw, _Opts) ->
    {ok, {read, Fd}};
open1(Name, Access, Raw, Opts) ->
    case file:open(Name, Raw ++ [binary, Access|Opts]) of
	{ok, File} ->
	    {ok, {Access, File}};
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

%%-------------------------------------------------------------------------
%% @doc Closes an ar archive.
%% @end
%%-------------------------------------------------------------------------
close({read, File}) ->
    ok = file:close(File);
close({write, File}) ->
    ok = file:close(File);
close(_) ->
    {error, einval}.

%%-------------------------------------------------------------------------
%% @doc Adds a file to an ar archive.
%% @end
%%-------------------------------------------------------------------------
%add(File, Name, Options) ->
%    add(File, Name, Name, Options).

add({write, File}, Name, NameInArchive, Options) ->
    Opts = #add_opts{read_info=fun(F) -> file:read_link_info(F) end},
    add1(File, Name, NameInArchive, add_opts(Options, Opts));
add({read, _File}, _, _, _) ->
    {error, eacces};
add(_, _, _, _) ->
    {error, einval}.

add_opts([dereference|T], Opts) ->
    add_opts(T, Opts#add_opts{read_info=fun(F) -> file:read_file_info(F) end});
add_opts([verbose|T], Opts) ->
    add_opts(T, Opts#add_opts{verbose=true});
add_opts([fakeroot|T], Opts) ->
    put(fakeroot, true),
    add_opts(T, Opts);
add_opts([_|T], Opts) ->
    add_opts(T, Opts);
add_opts([], Opts) ->
    Opts.


%%-------------------------------------------------------------------------
%% @doc Creates an ar file, from filename, containing the given files.
%% @end
%%-------------------------------------------------------------------------
create(Name, Filenames) ->
    create(Name, Filenames, []).

%%-------------------------------------------------------------------------
%% @doc Creates an ar archive, from filename, containing the given files.
%% @end
%% Accepted options: verbose, compressed, cooked
%%-------------------------------------------------------------------------
create(Name, FileList, Options) ->
    Mode = lists:filter(fun(X) -> (X=:=compressed) or (X=:=cooked) 
                        end, Options),
    put(fakeroot, lists:any(fun(F) -> F=:=fakeroot end, Options)),
    case open(Name, [write|Mode]) of
	{ok, ArFile} ->
	    % Add the magic number
	    {write,Port} = ArFile,
	    ok = file:write(Port, <<"!<arch>\n">>),
	    % Long filenames ?
	    ok = file:write(Port, long_filenames(FileList)), % Maybe nothing (0 byte)
	    Add = fun({NmInA, NmOrBin}) -> 
			  add(ArFile, NmOrBin, NmInA, Options);
		     (Nm) -> 
			  add(ArFile, Nm, Nm, Options)
		  end,
	    Result = foreach_while_ok(Add, FileList),
	    case {Result, close(ArFile)} of
		{ok, Res} -> Res;
		{Res, _} -> Res
	    end;
	Reason ->
	    Reason
    end.



%%-------------------------------------------------------------------------
%% @doc Extracts all files from the ar file given from filename.
%% @end
%%-------------------------------------------------------------------------
-spec extract(nonempty_string()) -> ok | {error,{nonempty_string(), term()}}.
extract(Name) ->
    extract(Name, []).

%%-------------------------------------------------------------------------
%% @doc Extracts (all) files from the ar file Name.
%% Options accepted: keep_old_files, {files, ListOfFilesToExtract}, verbose,
%%		{cwd, AbsoluteDirectory}
%% @end
%%-------------------------------------------------------------------------
-spec extract(nonempty_string(), list()) -> ok | {error,{nonempty_string(), term()}}.
extract(Name, Opts) ->
    foldl_read(Name, fun extract1/4, ok, extract_opts(Opts)).

%%-------------------------------------------------------------------------
%% @doc Returns a list of names of the files in the ar file given from filename.
%% Options accepted: verbose
%% @end
%%-------------------------------------------------------------------------
-spec table(nonempty_string()) -> {ok, list()}.
table(Name) ->
    table(Name, []).

%%-------------------------------------------------------------------------
%% @doc Returns a list of names of the files in the ar file from filename.
%% @end
%% Options accepted: compressed, verbose, cooked.
%%-------------------------------------------------------------------------
-spec table(nonempty_string(), list()) -> {ok, list()}.
table(Name, Opts) ->
    foldl_read(Name, fun table1/4, [], table_opts(Opts)).


%%-------------------------------------------------------------------------
%% @doc Comments for printing the contents of an ar archive,
%% meant to be invoked from the shell. Simple list of files.
%% @end
%%-------------------------------------------------------------------------
-spec t(nonempty_string()) -> ok.
t(Name) ->
    case table(Name) of
	{ok, List} ->
	    lists:foreach(fun(N) -> ok = io:format("~ts\n", [N]) end, lists:dropwhile(fun(X) -> case X of [] -> true ; _ -> false end end,  List));
	Error ->
	    Error
    end.

%%-------------------------------------------------------------------------
%% @doc Comments for printing the contents of an ar archive,
%% meant to be invoked from the shell. Long description.
%% @end
%%-------------------------------------------------------------------------
-spec tt(nonempty_string()) -> ok.
tt(Name) ->
    case table(Name, [verbose]) of
	{ok, List} ->
	    lists:foreach(fun print_header/1, lists:dropwhile(fun(X) -> case X of [] -> true ; _ -> false end end, List));
	Error ->
	    Error
    end.

%print_header({Name, Type, Size, Mtime, Mode, Uid, Gid}) ->
%    io:format("~s~s ~4w/~-4w ~7w ~s ~s\n",
%	      [type_to_string(Type), mode_to_string(Mode),
%	       Uid, Gid, Size, time_to_string(Mtime), Name]).
print_header([])-> ok;
print_header({Name, Size, Mtime, Mode, Uid, Gid}) ->
    io:format("~s ~4w/~-4w ~7w ~s ~s\n",
	      [ mode_to_string(Mode),
	       Uid, Gid, Size, time_to_string(Mtime), Name]).

%type_to_string(regular) -> "-";
%type_to_string(directory) -> "d";
%type_to_string(link) -> "l";
%type_to_string(symlink) -> "s";
%type_to_string(char) -> "c";
%type_to_string(block) -> "b";
%type_to_string(fifo) -> "f";
%type_to_string(_) -> "?".

mode_to_string(Mode) ->
    mode_to_string(Mode, "xwrxwrxwr", []).

mode_to_string(Mode, [C|T], Acc) when Mode band 1 =:= 1 ->
    mode_to_string(Mode bsr 1, T, [C|Acc]);
mode_to_string(Mode, [_|T], Acc) ->
    mode_to_string(Mode bsr 1, T, [$-|Acc]);
mode_to_string(_, [], Acc) ->
    Acc.

time_to_string({{Y, Mon, Day}, {H, Min, _}}) ->
    io_lib:format("~s ~2w ~s:~s ~w", [month(Mon), Day, two_d(H), two_d(Min), Y]).

two_d(N) ->
    tl(integer_to_list(N + 100)).

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%%-------------------------------------------------------------------------
%% @doc Converts the short error reason to a descriptive string.
%% @end
%%-------------------------------------------------------------------------


format_error(bad_header) -> "Bad directory header";
format_error(eof) -> "Unexpected end of file";
format_error(symbolic_link_too_long) -> "Symbolic link too long";
format_error({Name,Reason}) ->
    lists:flatten(io_lib:format("~ts: ~ts", [Name,format_error(Reason)]));
format_error(Atom) when is_atom(Atom) ->
    file:format_error(Atom);
format_error(Term) ->
    lists:flatten(io_lib:format("~tp", [Term])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	Useful definitions (also start of implementation).
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Offset for fields in the ar header.
%% Note that these offsets are ZERO-based as in the POSIX standard
%% document, while binaries use ONE-base offset.  Caveat Programmer.

-define(ah_name, 0).
-define(ah_mtime, 16).
-define(ah_uid, 28).
-define(ah_gid, 34).
-define(ah_mode, 40).
-define(ah_size, 48).
-define(ah_magic, 58).

-define(th_chksum, 148).
-define(th_typeflag, 156).
-define(th_linkname, 157).
-define(th_version, 263).
-define(th_prefix, 345).

%% Length of these fields.

-define(ah_name_len, 16).
-define(ah_mtime_len, 12).
-define(ah_uid_len, 6).
-define(ah_gid_len, 6).
-define(ah_mode_len, 8).
-define(ah_size_len, 10).
-define(ah_magic_len, 2).

-define(th_chksum_len, 8).
-define(th_linkname_len, 100).
-define(th_version_len, 2).
-define(th_prefix_len, 167).

-record(ar_header,
	{name,					% Name of file.
	 mtime,					% Last modified (seconds since Jan 1, 1970).
	 uid,					% User id.
	 gid,					% Group id.
	 mode,					% Mode bits.
	 size,					% Size of file
	 filler = "`\n"
	 }).				

%-define(record_size, 512).
%-define(block_size, (512*20)).
-define(record_size, 60).
-define(block_size,  (2)).

load_index(File, Size) -> 
    % Read Size length, then treat each filename
    Size2 = ?REM(Size),
    {ok, Data} = file:read(File, Size2),
    Blist = filename:split(Data),
    % Add 2 due to /\n at each end of filename ...
    {List2, _} = lists:mapfoldl(fun(X, Accin) ->  {{string:strip(binary_to_list(X), left, $\n), Accin}, Accin + 2 + size(X)} end, 0, Blist),
    lists:foreach(fun({N, O}) -> put(N, O), put(O, N) end, List2).
    

long_filenames(FileList) -> 
    % Get basenames
    Basenames = lists:flatmap(fun(B) -> [filename:basename(B)] end, FileList),
    % Basename filenames bigger than 15 characters (due to trailing / and 16 characters maxi)
    Longs = lists:filter(fun(X) -> (erlang:length(X) > 15) end , Basenames),
    % Add trailing / to long basenames
    Trailing = lists:flatmap(fun(T) -> [T ++ "/\n"] end, Longs),
    % Create index in process dictionnary
    longname_body_index(Trailing),
    case Trailing of
	  [] -> <<"">> ;
	  _  -> list_to_binary([longname_header(Trailing), longname_body(Trailing)]) 
    end.

longname_body_index(List) ->     
    % 2 indexes : name to offset and offset to name
    lists:foldl(fun(N, Acc) -> put(Acc, N), put(N, Acc), Acc + erlang:length(N) end, 0, List).

longname_header(Body) -> 
    Size = size(list_to_binary(Body)) + size(list_to_binary(padding(size(list_to_binary(Body)), 2))),
    H0 = [io_lib:format("~-"++ integer_to_list(?ah_name_len) ++"s",["//"]),
	  io_lib:format("~-"++ integer_to_list(?ah_mtime_len) ++"s",[""]),
	  io_lib:format("~-"++ integer_to_list(?ah_uid_len) ++"s",[""]),
	  io_lib:format("~-"++ integer_to_list(?ah_gid_len) ++"s",[""]),
	  io_lib:format("~-"++ integer_to_list(?ah_mode_len) ++"s",[""]),
	  io_lib:format("~-"++ integer_to_list(?ah_size_len) ++"s",[integer_to_list(Size)]),
	  "`\n"
	  ],
    H = list_to_binary(H0),
    60 = byte_size(H),		% Assertion.
    H.

longname_body(Trailing) -> list_to_binary(Trailing ++ padding(size(list_to_binary(Trailing)), 2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Adding members to an ar archive.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add1(ArFile, Bin, NameInArchive, Opts) when is_binary(Bin) ->
    Now = calendar:now_to_local_time(erlang:timestamp()),
    Info = #file_info{size = byte_size(Bin),
		      type = regular,
		      access = read_write,
		      atime = Now,
		      mtime = Now,
		      ctime = Now,
		      mode  = 8#100644,
		      links = 1,
		      major_device = 0,
		      minor_device = 0,
		      inode = 0,
		      uid = 0,
		      gid = 0},
    Header = create_header(NameInArchive, Info),
    add1(ArFile, NameInArchive, Header, Bin, Opts);
add1(ArFile, Name, NameInArchive, Opts) ->
    case read_file_and_info(Name, Opts) of
	{ok, Bin, Info} when Info#file_info.type =:= regular ->
	    Header = create_header(NameInArchive, Info),
	    add1(ArFile, Name, Header, Bin, Opts);
	{ok, PointsTo, Info} when Info#file_info.type =:= symlink ->
	    if
		length(PointsTo) > 100 ->
		    {error,{PointsTo,symbolic_link_too_long}};
		true ->
		    Info2 = Info#file_info{size=0},
		    Header = create_header(NameInArchive, Info2, PointsTo),
		    add1(ArFile, Name, Header, list_to_binary([]), Opts)
	    end;
	{ok, _, Info} when Info#file_info.type =:= directory ->
	    add_directory(ArFile, Name, NameInArchive, Info, Opts);
	{ok, _, #file_info{type=Type}} ->
	    {error, {bad_file_type, Name, Type}};
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

add1(Tar, Name, Header, Bin, Options) ->
    add_verbose(Options, "a ~ts~n", [Name]),
    file:write(Tar, [Header, Bin, padding(byte_size(Bin), 2)]).

add_directory(ArFile, DirName, NameInArchive, Info, Options) ->
    case file:list_dir(DirName) of
	{ok, []} ->
	    add_verbose(Options, "a ~ts~n", [DirName]),
	    Header = create_header(NameInArchive, Info),
	    file:write(ArFile, Header);
	{ok, Files} ->
	    Add = fun (File) ->
			  add1(ArFile,
			       filename:join(DirName, File),
			       filename:join(NameInArchive, File),
			       Options) end,
	    foreach_while_ok(Add, Files);
	{error, Reason} ->
	    {error, {DirName, Reason}}
    end.
    
%% Creates a header for file in an ar file.

create_header(Name, Info) ->
    create_header(Name, Info, []).
create_header(Name, #file_info {mode=Mode, uid=Uid, gid=Gid,
				size=Size, mtime=Mtime0, type=_Type}, _Linkname) ->
    Mtime = posix_time(erlang:localtime_to_universaltime(Mtime0)),
    Iuid = case get(fakeroot) of
	      true -> "0" ;
	      _    -> integer_to_list(Uid)
	    end,
    Igid = case get(fakeroot) of
	      true -> "0" ;
	      _    -> integer_to_list(Gid)
	    end,
    H0 = [io_lib:format("~-"++ integer_to_list(?ah_name_len) ++"s",[
	       % Change long name to index if needed
	      case get(Name++"/\n") of
		    undefined 		 -> % Assuming a short name
					    filename:basename(Name) ++ "/" ;
		    I when is_integer(I) -> "/" ++ integer_to_list(I) ;
		    _         		 -> filename:basename(Name) ++ "/" % should not go here
	      end
	  ]),
	  io_lib:format("~-"++ integer_to_list(?ah_mtime_len) ++"s",[integer_to_list(Mtime)]),
	  io_lib:format("~-"++ integer_to_list(?ah_uid_len) ++"s",[Iuid]),
	  io_lib:format("~-"++ integer_to_list(?ah_gid_len) ++"s",[Igid]),
	  io_lib:format("~-"++ integer_to_list(?ah_mode_len) ++".8B",[Mode]),
	  io_lib:format("~-"++ integer_to_list(?ah_size_len) ++"s",[integer_to_list(Size)]),
	  "`\n"
	  ],
    H = list_to_binary(H0),
    60 = byte_size(H),				%Assertion.
    H.

%file_type(regular) -> $0;
%file_type(symlink) -> $2;
%file_type(directory) -> $5.
    
%to_octal(Int, Count) when Count > 1 ->
%    to_octal(Int, Count-1, [0]).

%to_octal(_, 0, Result) -> Result;
%to_octal(Int, Count, Result) ->
%    to_octal(Int div 8, Count-1, [Int rem 8 + $0|Result]).

%to_string(Str0, Count) ->
%    Str = list_to_binary(Str0),
%    case byte_size(Str) of
%	Size when Size < Count ->
%	    [Str|zeroes(Count-Size)];
%	_ -> Str
%    end.

%% Pads out end of file.

%pad_file(File) ->
%    {ok,Position} = file:position(File, {cur,0}),
%    %% There must be at least one empty record at the end of the file.
%    Zeros = zeroes(?block_size - (Position rem ?block_size)),
%    file:write(File, Zeros).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Retrieving files from an ar archive.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Options used when reading an ar archive.

-record(read_opts,
	{cwd             :: string(),		% Current working directory.
	 keep_old_files = false :: boolean(),	% Owerwrite or not.
	 files = all,				% Set of files to extract
						% (or all).
	 output = file   :: 'file' | 'memory',
	 open_mode = [],			% Open mode options.
	 verbose = false :: boolean()}).	% Verbose on/off.

extract_opts(List) ->
    extract_opts(List, default_options()).

table_opts(List) ->
    read_opts(List, default_options()).

default_options() ->
    {ok, Cwd} = file:get_cwd(),
    #read_opts{cwd=Cwd}.

%% Parse options for extract.

extract_opts([keep_old_files|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{keep_old_files=true});
extract_opts([{cwd, Cwd}|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{cwd=Cwd});
extract_opts([{files, Files}|Rest], Opts) ->
    Set = ordsets:from_list(Files),
    extract_opts(Rest, Opts#read_opts{files=Set});
extract_opts([memory|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{output=memory});
extract_opts([compressed|Rest], Opts=#read_opts{open_mode=OpenMode}) ->
    extract_opts(Rest, Opts#read_opts{open_mode=[compressed|OpenMode]});
extract_opts([cooked|Rest], Opts=#read_opts{open_mode=OpenMode}) ->
    extract_opts(Rest, Opts#read_opts{open_mode=[cooked|OpenMode]});
extract_opts([verbose|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{verbose=true});
extract_opts([Other|Rest], Opts) ->
    extract_opts(Rest, read_opts([Other], Opts));
extract_opts([], Opts) ->
    Opts.

%% Common options for all read operations.

read_opts([compressed|Rest], Opts=#read_opts{open_mode=OpenMode}) ->
    read_opts(Rest, Opts#read_opts{open_mode=[compressed|OpenMode]});
read_opts([cooked|Rest], Opts=#read_opts{open_mode=OpenMode}) ->
    read_opts(Rest, Opts#read_opts{open_mode=[cooked|OpenMode]});
read_opts([verbose|Rest], Opts) ->
    read_opts(Rest, Opts#read_opts{verbose=true});
read_opts([_|Rest], Opts) ->
    read_opts(Rest, Opts);
read_opts([], Opts) ->
    Opts.

foldl_read(ArName, Fun, Accu, Opts) ->
    case open(ArName, [read|Opts#read_opts.open_mode]) of
	{ok, {read, File}} ->
            % Skip the magic number in beginning of file_info
	    {ok, <<"!<arch>\n">>} = file:read(File, 8),
	    Result = 
		case catch foldl_read1(Fun, Accu, File, Opts) of
		    {'EXIT', Reason} ->
			exit(Reason);
		    {error, {Reason, Format, Args}} ->
			read_verbose(Opts, Format, Args),
			{error, Reason};
		    {error, Reason} ->
			{error, Reason};
		    Ok ->
			Ok
		end,
	    ok = file:close(File),
	    Result;
	Error ->
	    Error
    end.

foldl_read1(Fun, Accu0, File, Opts) ->
    case get_header(File) of
	eof ->
	    Fun(eof, File, Opts, Accu0);
	Header ->
	    {ok, NewAccu} = Fun(Header, File, Opts, Accu0),
	    foldl_read1(Fun, NewAccu, File, Opts)
    end.

table1(eof, _, _, Result) ->
    {ok, lists:dropwhile(fun(X) -> case X of [] -> true ; _ -> false end end, lists:reverse(Result))};
table1(#ar_header{name="//", size=Size}, File, _, Result) ->   
    load_index(File, Size),
    {ok, [Result]};
table1(Header = #ar_header{}, File, #read_opts{verbose=true}, Result) ->
    #ar_header{name=Name, size=Size, mtime=Mtime, 
		mode=Mode, uid=Uid, gid=Gid} = Header,  
    Size2 = ?REM(Size),
    skip(File, Size2),
    {ok, [{Name, Size, posix_to_erlang_time(Mtime), Mode, Uid, Gid}|Result]};
table1(#ar_header{name=Name, size=Size}, File, _, Result) ->    
    Size2 = ?REM(Size),
    skip(File, Size2),
    {ok, [Name|Result]}.

extract1(eof, _, _, Acc) ->
    if
	is_list(Acc) ->
	    {ok, lists:reverse(Acc)};
	true ->
	    Acc
    end;
extract1(Header, File, Opts, Acc) ->
    Name = Header#ar_header.name,
    case check_extract(Name, Opts) of		
	true ->
	    {ok, Bin} = get_element(File, Header),
	    case write_extracted_element(Header, Bin, Opts) of
		ok ->
		    {ok, Acc};
		{ok, NameBin} when is_list(Acc) ->
		    {ok, [NameBin | Acc]};
		{ok, NameBin} when Acc =:= ok ->
		    {ok, [NameBin]}
	    end;
	false ->   
		 Size = ?REM(Header#ar_header.size),
		 ok = skip(File, Size),
		 {ok, Acc};
	index -> % Load index
		 load_index(File, Header#ar_header.size),
		 {ok, Acc}
    end.

%% Checks if the file Name should be extracted.
check_extract("//", _) ->
    index;
check_extract(_, #read_opts{files=all}) ->
    true;
check_extract(Name, #read_opts{files=Files}) ->
    ordsets:is_element(Name, Files).

get_header(File) ->
    case file:read(File, ?record_size) of
	eof ->
	    %throw({error,eof});
	    eof ;
	{ok, Bin} when is_binary(Bin) ->
	    convert_header(Bin);
	{ok, List} ->
	    convert_header(list_to_binary(List));
	{error, Reason} ->
	    throw({error, Reason})
    end.

%% Converts the ar header to a record.

convert_header(Bin) when byte_size(Bin) =:= ?record_size ->
	    Name = get_name(Bin),
	    Hd = case Name of
		      "//" ->  % Pseudo Header for index 
			      #ar_header{ name="//",
					  mtime=0,
					  uid=0,
					  gid=0,
					  mode=0,
					  size=from_integer(from_string(Bin, ?ah_size, ?ah_size_len)),
					  filler="`\n"
					} ;	
		      _    -> 
			      #ar_header{ name=Name,
					  mtime=from_integer(from_string(Bin, ?ah_mtime, ?ah_mtime_len)),
					  uid=from_integer(from_string(Bin, ?ah_uid, ?ah_uid_len)),
					  gid=from_integer(from_string(Bin, ?ah_gid, ?ah_gid_len)),
					  mode=from_octal(Bin, ?ah_mode, ?ah_mode_len),
					  size=from_integer(from_string(Bin, ?ah_size, ?ah_size_len)),
					  filler="`\n"
					}	    
		end,
            %?DEBUG(Hd),
	    Hd;

convert_header(Bin) when byte_size(Bin) =:= 0 ->
    eof;
convert_header(_Bin) ->
    throw({error, eof}).

%% Basic sanity.  Better set the element size to zero here if the type
%% always is of zero length.

%convert_header1(H) when H#ar_header.size =/= 0 ->
%    convert_header1(H#ar_header{size=0});
%convert_header1(H) when H#ar_header.size =/= 0 ->
%    convert_header1(H#ar_header{size=0});
%convert_header1(Header) ->
%    Header.


%% Get the name of the file from the prefix and name fields of the ar header.

get_name(Bin) ->
    Name = case filename:split(from_string(Bin, ?ah_name, ?ah_name_len)) of
	      ["/", "              "] 	-> % Longname Index (//)
					    "//";
	      ["/", Int] 		-> % Longname offset (/15)
					    get(list_to_integer(string:strip(Int)));
	      [Left] 			-> Left;
	      [Left, _]			-> Left
	   end,
    Name.

from_string(Bin, Pos, Len) ->
    lists:reverse(remove_nulls(binary_to_list(Bin, Pos+1, Pos+Len))).
    
%% Returns all characters up to (but not including) the first null
%% character, in REVERSE order.

remove_nulls(List) ->
    remove_nulls(List, []).

remove_nulls([0|_], Result) ->
    remove_nulls([], Result);
remove_nulls([C|Rest], Result) ->
    remove_nulls(Rest, [C|Result]);
remove_nulls([], Result) ->
    Result.

from_integer(S) ->
      {Int, _Rest} = string:to_integer(S),
      Int.

from_octal(Bin, Pos, Len) ->
    from_octal(binary_to_list(Bin, Pos+1, Pos+Len)).

from_octal([$\s|Rest]) ->
    from_octal(Rest);
from_octal([Digit|Rest]) when $0 =< Digit, Digit =< $7 ->
    from_octal(Rest, Digit-$0);
from_octal(Bin) when is_binary(Bin) ->
    from_octal(binary_to_list(Bin));
from_octal(Other) ->
    throw({error, {bad_header, "Bad octal number: ~p", [Other]}}).

from_octal([Digit|Rest], Result) when $0 =< Digit, Digit =< $7 ->
    from_octal(Rest, Result*8+Digit-$0);
from_octal([$\s|_], Result) ->
    Result;
from_octal([0|_], Result) ->
    Result;
from_octal(Other, _) ->
    throw({error, {bad_header, "Bad contents in octal field: ~p", [Other]}}).

%% Retrieves the next element from the archive.
%% Returns {ok, Bin} | eof | {error, Reason}

get_element(File, #ar_header{size = 0}) ->
    skip_to_next(File,0),
    {ok,<<>>};
get_element(File, #ar_header{size = Size}) ->
    case file:read(File, Size) of
	{ok,Bin}=Res when byte_size(Bin) =:= Size ->
	    %skip_to_next(File, Size),
	    Size2 = ?REM01(Size),
	    skip(File, Size2),
	    Res;
	{ok,List} when length(List) =:= Size ->
	    %skip_to_next(File, Size),
	    Size2 = ?REM01(Size),
	    skip(File, Size2),
	    {ok,list_to_binary(List)};
	{ok,_} -> throw({error,eof});
	{error, Reason} -> throw({error, Reason});
	eof -> throw({error,eof})
    end.


write_extracted_element(Header, Bin, Opts)
  when Opts#read_opts.output =:= memory ->    
	    {ok, {Header#ar_header.name, Bin}};
write_extracted_element(Header, Bin, Opts) ->
    Name = filename:absname(Header#ar_header.name, Opts#read_opts.cwd),
    Created = write_extracted_file(Name, Bin, Opts),
    case Created of
	ok  -> set_extracted_file_info(Name, Header);
	not_written -> ok
    end.


write_extracted_file(Name, Bin, Opts) ->
    Write =
	case Opts#read_opts.keep_old_files of
	    true ->
		case file:read_file_info(Name) of
		    {ok, _} -> false;
		    _ -> true
		end;
	    false -> true
	end,
    case Write of
	true ->
	    read_verbose(Opts, "x ~ts~n", [Name]),
	    write_file(Name, Bin);
	false ->
	    read_verbose(Opts, "x ~ts - exists, not created~n", [Name]),
	    not_written
    end.

write_file(Name, Bin) ->
    case file:write_file(Name, Bin) of
	ok -> ok;
	{error,enoent} ->
	    ok = make_dirs(Name, file),
	    write_file(Name, Bin);
	{error,Reason} ->
	    throw({error, Reason})
    end.

%set_extracted_file_info(_, #ar_header{typeflag = symlink}) -> ok;
set_extracted_file_info(Name, #ar_header{mode=Mode, mtime=Mtime}) ->
    Info = #file_info{mode=Mode, mtime=posix_to_erlang_time(Mtime)},
    file:write_file_info(Name, Info).

%% Makes all directories leading up to the file.

make_dirs(Name, file) ->
	filelib:ensure_dir(Name);
make_dirs(Name, dir) ->
	filelib:ensure_dir(filename:join(Name,"*")).

%% Prints the message on if the verbose option is given (for reading).

read_verbose(#read_opts{verbose=true}, Format, Args) ->
    io:format(Format, Args),
    io:nl();
read_verbose(_, _, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Utility functions.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Returns a list of zeroes to pad out to the given block size.

padding(Size, BlockSize) ->
    fill_lf(pad_size(Size, BlockSize)).

pad_size(Size, BlockSize) ->
    case Size rem BlockSize of
	0 -> 0;
	Rem -> BlockSize-Rem
    end.

fill_lf(0) -> [];
fill_lf(1) -> "\n".

%% Skips the given number of bytes rounded up to an even record.

skip(File, Size) ->
    %% Note: There is no point in handling failure to get the current position
    %% in the file.  If it doesn't work, something serious is wrong.
    {ok, Position} = file:position(File, {cur, 0}),
    {ok, _NewPosition} = file:position(File, Position + Size),
    ok.

%% Skips to the next record in the file.

skip_to_next(File, Size) ->
    %% Note: There is no point in handling failure to get the current position
    %% in the file.  If it doesn't work, something serious is wrong.
    {ok, Position} = file:position(File, {cur, 0}),
    NewPosition = case (Size rem 2) of
		    0 -> Position + Size ;
		    1 -> Position + Size + 1
		  end,
    %NewPosition = ((Position + ?record_size - 1) div ?record_size) * ?record_size,
    {ok, NewPosition} = file:position(File, NewPosition),
    ok.

%% Prints the message on if the verbose option is given.

add_verbose(#add_opts{verbose=true}, Format, Args) ->
    io:format(Format, Args);
add_verbose(_, _, _) ->
    ok.

%% Converts a tuple containing the time to a Posix time (seconds
%% since Jan 1, 1970).

posix_time(Time) ->
    EpochStart = {{1970,1,1},{0,0,0}},
    {Days,{Hour,Min,Sec}} = calendar:time_difference(EpochStart, Time),
    86400*Days + 3600*Hour + 60*Min + Sec.

posix_to_erlang_time(Sec) ->
    OneMillion = 1000000,
    Time = calendar:now_to_datetime({Sec div OneMillion, Sec rem OneMillion, 0}),
    erlang:universaltime_to_localtime(Time).

read_file_and_info(Name, Opts) ->
    ReadInfo = Opts#add_opts.read_info,
    case ReadInfo(Name) of
	{ok,Info} when Info#file_info.type =:= regular ->
	    case file:read_file(Name) of
		{ok,Bin} ->
		    {ok,Bin,Info};
		Error ->
		    Error
	    end;
	{ok,Info} when Info#file_info.type =:= symlink ->
	    case file:read_link(Name) of
		{ok,PointsTo} ->
		    {ok,PointsTo,Info};
		Error ->
		    Error
	    end;
	{ok, Info} ->
	    {ok,[],Info};
	Error ->
	    Error
    end.

foreach_while_ok(Fun, [First|Rest]) ->
    case Fun(First) of
	ok -> foreach_while_ok(Fun, Rest);
	Other -> Other
    end;
foreach_while_ok(_, []) -> ok.
    
open_mode(Mode) ->
    open_mode(Mode, false, [raw], []).

open_mode(read, _, Raw, _) ->
    {ok, read, Raw, []};
open_mode(write, _, Raw, _) ->
    {ok, write, Raw, []};
open_mode([read|Rest], false, Raw, Opts) ->
    open_mode(Rest, read, Raw, Opts);
open_mode([write|Rest], false, Raw, Opts) ->
    open_mode(Rest, write, Raw, Opts);
open_mode([compressed|Rest], Access, Raw, Opts) ->
    open_mode(Rest, Access, Raw, [compressed|Opts]);
open_mode([cooked|Rest], Access, _Raw, Opts) ->
    open_mode(Rest, Access, [], Opts);
open_mode([], Access, Raw, Opts) ->
    {ok, Access, Raw, Opts};
open_mode(_, _, _, _) ->
    {error, einval}.
