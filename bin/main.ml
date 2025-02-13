type result_parse_line = Exit | Invalid | Point of (float * float)
type interpolation_method = Linear | Newton

let parse_line line =
  try
    let parts = String.split_on_char ';' line in
    match parts with
    | [ x_str; y_str ] ->
        let x = float_of_string x_str in
        let y = float_of_string y_str in
        Point (x, y)
    | "q" :: [] -> Exit
    | _ -> Invalid
  with _ -> Invalid

let generate_x x0 x1 step =
  let rec loop current acc =
    if current > x1 then List.rev acc
    else loop (current +. step) (current :: acc)
  in
  let result = loop x0 [] in
  result

let interpolate_linear x0 y0 x1 y1 step =
  generate_x x0 x1 step
  |> List.map (fun x -> (x, y0 +. ((y1 -. y0) *. (x -. x0) /. (x1 -. x0))))

let divided_differences points =
  let n = List.length points in
  let table = Array.make_matrix n n 0.0 in
  List.iteri (fun i (_, y) -> table.(i).(0) <- y) points;
  for j = 1 to n - 1 do
    for i = 0 to n - j - 1 do
      let xi, _ = List.nth points i in
      let xj, _ = List.nth points (i + j) in
      table.(i).(j) <- (table.(i + 1).(j - 1) -. table.(i).(j - 1)) /. (xj -. xi)
    done
  done;
  Array.to_list (Array.map (fun row -> row.(0)) table)

let interpolate_newton points step =
  if List.length points < 2 then []
  else
    let x_max = fst (List.hd points) in
    let x_min = fst (List.nth points (List.length points - 1)) in
    let x_list = generate_x x_min x_max step in

    let newton_interpolation x =
      let coeffs = divided_differences points in
      let rec eval_poly coeffs x base acc =
        match coeffs with
        | [] -> acc
        | c :: cs ->
            eval_poly cs x
              (base *. (x -. fst (List.hd points)))
              (acc +. (c *. base))
      in
      eval_poly coeffs x 1.0 0.0
    in
    List.map (fun x -> (x, newton_interpolation x)) x_list

let truncate_list n lst =
  let rec take n acc = function
    | [] -> List.rev acc
    | _ when n = 0 -> List.rev acc
    | x :: xs -> take (n - 1) (x :: acc) xs
  in
  take n [] lst

let process_input step method_name window_size =
  let rec loop last_points =
    try
      let line = read_line () in
      match parse_line line with
      | Invalid ->
          Printf.printf "Invalid input\n";
          loop last_points
      | Exit -> ()
      | Point (x, y) ->
          let new_points = truncate_list window_size ((x, y) :: last_points) in
          let interpolated =
            match method_name with
            | Linear when List.length new_points >= 2 ->
                let x0, y0 = List.nth new_points 1 in
                interpolate_linear x0 y0 x y step
            | Newton when List.length new_points >= 2 ->
                interpolate_newton new_points step
            | _ -> []
          in
          if interpolated <> [] then
            List.iter
              (fun (x, y) -> Printf.printf "%.2f %.2f\n" x y)
              interpolated;
          loop new_points
    with End_of_file -> ()
  in
  loop []

let main () =
  if Array.length Sys.argv < 4 then
    failwith
      "Usage: ./functional_programming_lab3 <step> <method> <window_size>"
  else
    let step =
      try float_of_string Sys.argv.(1) with _ -> failwith "Invalid step"
    in
    let method_str = Sys.argv.(2) in
    let interpolation_method =
      match method_str with
      | "linear" -> Linear
      | "newton" -> Newton
      | _ -> failwith "Invalid interpolation method"
    in
    let window_size =
      try int_of_string Sys.argv.(3) with _ -> failwith "Invalid window size"
    in
    process_input step interpolation_method window_size

let () = main ()
