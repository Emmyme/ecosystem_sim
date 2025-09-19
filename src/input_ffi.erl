-module(input_ffi).
-export([read_line/1]).

% Read a line of input from the user
read_line(Prompt) ->
    io:format("~s", [Prompt]),
    case io:get_line("") of
        eof -> 
            {error, "End of input"};
        {error, Reason} -> 
            {error, atom_to_list(Reason)};
        Line -> 
            TrimmedLine = string:trim(Line, trailing, "\n\r"),
            {ok, TrimmedLine}
    end.