module IntMap = 
struct
    module M = Map.Make (Int)
    type t = string

    let add = M.add
    let empty = M.empty
    let find_opt = M.find_opt
end

let parse_string (str: string) (direction: int): string IntMap.M.t =
    let map_int_str = ref IntMap.empty in
    let rec apply_each (fn: string -> unit) (li: string list) =
        match li with
        | [] -> ()
        | x :: xs -> fn x; apply_each fn xs in
    let nums = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] in
    let handle_num (cur_str: string): unit =
        let regex = Str.regexp cur_str in
        let found_idx = match direction with
        | -1 -> Str.search_backward regex str (String.length str)
        | _ -> Str.search_forward regex str 0 in
        map_int_str := IntMap.add found_idx cur_str !map_int_str in
    apply_each handle_num nums;

let traverse (str: string) (step: int): string = 
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

let read_lines (filename: string): int =
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
