module IntMap = 
struct
    module M = Map.Make (Int)

    let add = M.add
    let empty = M.empty
    let find_opt = M.find_opt
end

let parse_string str =
    let nums = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] in
    let handle_num cur_str =
        let found_idx = 

let traverse str step = 
    let idx = match step with
        | -1 -> String.length str - 1
        | _ -> 0 in
    let rec process str idx = 
        if (idx < 0) || (idx >= (String.length str)) then ();
        let parse_char char = match char with
            | '0' .. '9' -> String.make 1 char
            | _ -> process str (idx + step) in
        parse_char str.[idx] in
    process str idx

let read_lines filename =
    let total = ref 0 in
    let stream = open_in filename in
    let read () = 
        try Some (input_line stream) with End_of_file -> None in
    let rec get_lines () = match read () with
        | Some s -> let calibration = (traverse s 1) ^ (traverse s (-1)) |> int_of_string in
                total := !total + calibration; get_lines()
        | None -> close_in stream in
    get_lines ();
    !total

let () =
    let total = read_lines "input.txt" in
    string_of_int total ^ "\n" |> print_string
