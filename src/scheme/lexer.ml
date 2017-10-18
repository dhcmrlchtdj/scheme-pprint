type t = {
    mutable chr: char list;
    chan: in_channel;
}
let advance stm =
    match stm.chr with
        | [] ->
            input_char stm.chan
        | c::rest ->
            stm.chr <- rest;
            c
let unread_char stm c = stm.chr <- c :: stm.chr
