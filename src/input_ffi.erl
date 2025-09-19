-module(input_ffi).
-export([read_line/1]).

% Read a line of input from the user with a prompt
read_line(Prompt) ->
    % Print the prompt without a newline
    io:format("~s", [Prompt]),
    % Read a line of input and remove the trailing newline
    case io:get_line("") of
        eof -> 
            {error, "End of input"};
        {error, Reason} -> 
            {error, atom_to_list(Reason)};
        Line -> 
            % Remove trailing newline and return as a string
            TrimmedLine = string:trim(Line, trailing, "\n\r"),
            {ok, TrimmedLine}
    end.