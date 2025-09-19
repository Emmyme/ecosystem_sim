// External function to read user input from Erlang
@external(erlang, "input_ffi", "read_line")
fn read_line_external(prompt: String) -> Result(String, String)

// Read input from the user
pub fn read_line(prompt: String) -> Result(String, String) {
  read_line_external(prompt)
}

// Read input with error handling and default
pub fn read_line_with_default(prompt: String, default: String) -> String {
  case read_line(prompt) {
    Ok(input) ->
      case input {
        "" -> default
        _ -> input
      }
    Error(_) -> default
  }
}
