type job = {
  duration : int;
  start_time : int;
  priority : int;
}

let read_jobs () =
  let list_jobs = [];
  for job_num = 0 to num_jobs do
    list_jobs @ [ask_information job_num]
  done;

  Printf.printf "Choose a scheduling strategy (1 for No Overlaps, 2 for Max Priority, 3 for Minimize Idle Time): ";
  let scheduling_strategy = input_line stdin in print_endline scheduling_strategy;

let ask_information job_num = 
  (* We were reminded by ChatGPT to open the stdin, as the example in OCaml documentation did not include this *)
  Printf.printf "For job %d, please enter the following details:\nStart Time (hours): " job_num;
  let start_time_hrs = input_line stdin in
  
  Printf.printf "Start Time (minutes): ";
  let start_time_mins = input_line stdin in

  Printf.printf "Duration (minutes): ";
  let duration = input_line stdin in
  
  Printf.printf "Priority: ";
  let priority = input_line stdin in

  { duration = (int_of_string duration); start_time = (time_to_minutes start_time_hrs start_time_mins); priority = (int_of_string priority) }

(* Main method down here: *)
Printf.printf "How many jobs do you want to schedule? ";
let num_jobs = input_line stdin in
let () = print_endline (read_jobs num_jobs)