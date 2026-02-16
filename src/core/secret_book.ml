type book_state = {
  mutable read : bool;
}

let create_state () = {
  read = false;
}

let mark_as_read state =
  state.read <- true

let is_read state =
  state.read
