type job = {
  duration : int;
  start_time : int;
  priority : int;
}


(* 
* This recursive function reads the number of jobs
* 
* Return the number of jobs `num_jobs`
*)
let rec get_job_number() = 
  print_string "How many jobs do you want to schedule? ";
  
  (* Read the input from user and store it in `input` *)
  let input = read_line () in

  try
    (* Convert the `input` from string to integer *)
    let num_jobs = int_of_string input in

    if num_jobs < 0 then(
      print_endline "Please enter a positive integer.";
      get_job_number()
    )

    else if num_jobs = 0 then(
      print_endline "You have not scheduled any jobs.\n";

      (* Exit the program with status code 0 *)
      exit 0
    )

    else
      (* Return `num_jobs` *)
      num_jobs

    with
    | Failure _ ->
      print_endline "Invalid input. Please enter a non-negative integer.";
      get_job_number()


let time_to_minutes hrs mins = 
  (int_of_string mins) +   (int_of_string hrs)*60


(* 
* This function prompts the user to input job details
*
* job_num: A number label for the job
*
* Return a `job` type variable 
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
  (* Syntax for this function was corrected by ChatGPT, allowing use of ; symbol and keyword in properly *)

(* 
* This function calls ask_information() in a for loop
*
* num_jobs: The total number of jobs that entered by user
*
* Return a list of job: `list_jobs`
*)
let read_jobs num_jobs =
  let list_jobs = ref [] in
  for job_num = 1 to num_jobs do
    list_jobs := !list_jobs @ [ask_information job_num]
  done;
   !list_jobs
  (* Syntax for this function was corrected by ChatGPT, allowing use of a ref object properly *)


(* 
* This recursive function prompts the user to choose a job scheduling strategy.
*
* Return the option number that entered by user.
*)
let rec scheduling_strategy_option() =
  print_string "\nChoose a scheduling strategy (1 for No Overlaps, 2 for Max Priority, 3 for Minimize Idle Time):";

  (* Read the input from user and store it in `input` *)
  let input = read_line() in

  try
    (* Convert the `input` from string to integer *)
    let num_option = int_of_string input in

    if num_option = 1 || num_option = 2 || num_option = 3 then(
      
      (* Return `num_jobs` *)
      num_option
    )

    else(
      print_endline "Invalid number. Please enter 1 or 2 or 3.";

      (* call scheduling_strategy_option() recursively *)
      scheduling_strategy_option()
    )

  with
  | Failure _ ->
    print_endline "Invalid input. Please enter an integer.";
    scheduling_strategy_option()


(*Auxiliary function for sorting jobs by starttimes*)
let sort_by_start_time jobs = 
  List.sort (fun j1 j2 -> (compare j1.start_time j2.start_time)) jobs;;


(* 
* This function is to compare jobs by start time, and by priority
* if start time are equal.
*
*)
let compare_by_start_time_and_priority job1 job2 =
  (* Compare the jobs by start time *)
  let start_time_diff = compare job1.start_time job2.start_time in

  if start_time_diff = 0 then
    (* 
    * Compare the jobs by priority if start time are equal 
    * 
    * Return A negative integer (< 0) if job1.priority < job2.priority (i.e., job1 has higher priority).
    * Return Zero (0) if both jobs have the same priority.
    * Return A positive integer (> 0) if job1.priority > job2.priority (i.e., job2 has higher priority).
    * compare job1.priority job2.priority
    *)
    compare job1.priority job2.priority

  else
    (* Else return the result of the comparison by start time *)
    start_time_diff


(*No overlap*)
let no_overlab_schedule jobs =
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
          if job.start_time >= (last_job.start_time + last_job.duration) then (*Checks if next job starts after the previous job finishes or at exactly when the job finishes*)
            overlap(job::scheduled) tail (*Schedule the current job if no overlap*)
          else
            overlap scheduled tail in (*skip current job due to overlap*)
  List.rev (overlap[]sorted) (*reverse the current job since the new jobs are added to the front *)


(* 
* This function reads the job list and then schedule the job
* by their priority.
* 
* Each job is assigned a numerical priority, with a lower number
* indicating a higher priority.
* 
* If any jobs have the same start time, sort the jobs by priority.
* i.e. Schedule the job first if they have higher priority.
* 
* If two jobs have the same start time and priority, start one of
* them, then start another one until the first one is finished.

* Return a list of scheduled job: `scheduled_jobs`
*)
let max_priority_schedule (job_list) = 
  (* 
  * Sort the jobs by start time, and by priority if start time
  * are equal in ascending order 
  *)
  let scheduled_jobs = List.sort compare_by_start_time_and_priority job_list in

  (* Return a list of scheduled job *)
  scheduled_jobs


(*Minimize idle time *)
let min_idle_time_schedule jobs = 
  let no_overlap = no_overlab_schedule jobs in (*Make sure no jobs overlap*)
  let sorted = sort_by_start_time no_overlap in (*Sort the jobs by start time to minimize idle time*)
  sorted


(* 
* This function calls scheduling_strategy_option() to retrieve
* the option number for scheduling strategy, then it reads the
* job list given by the user and finally prints the scheduled
* job list accroding to the selected schedule strategy.
* 
* Return None
*)
let print_scheduled_jobs (job_list) =
  (* Retrieve the option number for scheduling strategy *)
  let option_num = scheduling_strategy_option() in

  let option_str = 
    if option_num = 1 then
      "No Overlaps"
    else if option_num = 2 then
      "Max Priority"
    else
      "Minimize Idle Time"
  in

  let scheduled_job_list = 
    if option_num = 1 then
      no_overlab_schedule (job_list)
    else if option_num = 2 then
      max_priority_schedule (job_list)
    else
      (* if option_num = 3 then *)
      min_idle_time_schedule (job_list)
  in

  Printf.printf "\nScheduled Jobs (%s):\n" option_str;

  List.iter (fun job ->
    Printf.printf "Job scheduled: Start Time = %d minutes, Duration =  %d minutes, Priority = %d\n"
    job.start_time job.duration job.priority
  ) scheduled_job_list;

  print_newline ()


(* Main program *)
let () = 
  (* Prompt the user for the number of jobs *)
  let job_num = get_job_number() in

  (* Prompt the user for the details of a list of jobs *)
  let job_list = read_jobs (job_num) in

  print_scheduled_jobs job_list;