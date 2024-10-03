type job = {
  duration : int;
  start_time : int;
  priority : int;
}


(* 
* This recursive function reads the number of jobs
* 
* returns the number of jobs `num_jobs`
*)
let rec job_number() = 
  print_string "How many jobs do you want to schedule? ";
  
  (* Read the input from user and store it in `input` *)
  let input = read_line () in

  try
    (* Convert the `input` from string to integer *)
    let num_jobs = int_of_string input in

    if num_jobs < 0 then(
      print_endline "Please enter a positive integer.";
      job_number()
    )

    else if num_jobs = 0 then(
      print_endline "You have not scheduled any jobs";

      (* Return `num_jobs` *)
      num_jobs
    )

    else
      num_jobs

    with
    | Failure _ ->
      print_endline "Invalid input. Please enter a non-negative integer.";
      job_number()


let time_to_minutes hrs mins = 
  (int_of_string mins) +   (int_of_string hrs)*60


(* 
* This function prompts the user to input job details
*
* job_num: A number label for the job
*
* returns a `job` type variable 
*)
let ask_information job_num = 
  Printf.printf "For job %d, please enter the following details:\nStart Time (hours): " job_num;
  let start_time_hrs = read_line () in
  
  Printf.printf "Start Time (minutes): ";
  let start_time_mins = read_line () in

  Printf.printf "Duration (minutes): ";
  let duration = read_line () in
  
  Printf.printf "Priority: ";
  let priority = read_line () in

  { duration = (int_of_string duration); start_time = (time_to_minutes start_time_hrs start_time_mins); priority = (int_of_string priority) }


(* 
* This function calls ask_information() in a for loop
*
* num_jobs: The total number of jobs that entered by user
*
* returns a list of job: `list_jobs`
*)
let read_jobs num_jobs =
  let list_jobs = ref [] in
  for job_num = 1 to num_jobs do
    list_jobs := !list_jobs @ [ask_information job_num]
  done;
   !list_jobs
  (* Syntax for this function was corrected by ChatGPT, allowing use of a ref object properly *)



(*Auxiliary function for sorting jobs by starttimes*)
let sort_by_start_time jobs = 
  List.sort (fun j1 j2 -> (compare j1.start_time j2.start_time)) jobs;;

(*No overlap*)
let schedule_jobs jobs=   
  (*Sort the jobs by start time in ascending order*)
  let sorted = sort_by_start_time jobs in (*The let ... in expression of defining the scope of a variable is explained by chatGPT *)
  (*Auxiliary function for checking overlap*)
  let rec overlap scheduled remaining = 
    match remaining with
    | [] -> scheduled (*if no remaining jobs left,  return scheduled jobs*)
    | job::tail ->    (*retrieve head of remaining jobs*)
        match scheduled with
        | [] -> overlap [job] tail (*if no jobs are scheduled, schedule the head job*)
        | last_job :: _ -> (*Check for overlap*)
          if job.start_time >= (last_job.start_time + last_job.duration) then (*Checks if next job starts after the previous job fnishes or at exactly when the job finishes*)
            overlap(job::scheduled) tail (*Schedule the current job if no overlap*)
          else
            overlap scheduled tail in (*skip current job due to overlap*)
  List.rev (overlap[]sorted) (*reverse the current job since the new jobs are added to the front *)


(*Minimize idle time *)
let minimize_idle_time jobs = 
  let no_overlap = schedule_jobs jobs in (*Make sure no jobs overlap*)
  let sorted = sort_by_start_time no_overlap in (*Sort the jobs by start time to minimize idle time*)
  sorted

(*Main method down here: *)

(* Printf.printf "How many jobs do you want to schedule? "; *)

(* DEBUG: DELETE LATER*)
let print_job_list job_list =
  List.iter (fun job ->
    Printf.printf "Duration: %d minutes, Start Time: %d minutes, Priority: %d\n"
      job.duration job.start_time job.priority
  ) job_list


(* Main program *)
let () = 
  (*Test no overlap*)
  Printf.printf "Test no overlap";
  Printf.printf "\n";
  let jobs = [
  { start_time = 570; duration = 60;priority = 3};
  { start_time = 660; duration = 45;priority = 5};
  ]in

  let tst = schedule_jobs jobs in
  print_job_list tst;
  Printf.printf "------------------------------------------";
  Printf.printf "Test min idle time";
  Printf.printf "\n";
  (*Test minimize idle time*)
  let jobs = [
  { start_time = 540; duration = 60; priority = 2 };  
  { start_time = 510; duration = 20; priority = 2 };
  { start_time = 520; duration = 45; priority = 3 };  
  { start_time = 610; duration = 30; priority = 2 };  
  ]in

  let min_idle_time = minimize_idle_time jobs in

  print_job_list min_idle_time;

  Printf.printf "-------------------------------------------";
  (*Actual main running*)
  (*Prompt the user for input*)
  let job_num = job_number() in
  let job_list = read_jobs job_num in
  
  let test = schedule_jobs job_list in (*Test schedule_jobs on user input*)

  print_job_list test;
 
  (* DEBUG: DELETE LATER*)
  (* print_job_list job_list; *)


(* Main method down here: *)
(* Printf.printf "How many jobs do you want to schedule? ";
let num_jobs = input_line stdin in
let () = (read_jobs num_jobs)

Printf.printf "Choose a scheduling strategy (1 for No Overlaps, 2 for Max Priority, 3 for Minimize Idle Time): ";
let scheduling_strategy = input_line stdin in print_endline scheduling_strategy; *)