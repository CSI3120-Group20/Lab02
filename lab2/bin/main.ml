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
      print_endline "You have not scheduled any jobs";

      (* Return `num_jobs` *)
      num_jobs
    )

    else
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
  (* Syntax for this function was corrected by ChatGPT, allowing use of ; symbol and keyword in properly *)

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
          if job.start_time >= (last_job.start_time + last_job.duration) then (*Checks if next job starts after the previous job finishes or at exactly when the job finishes*)
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
  Printf.printf "-------------------------------------------------\n";
  Printf.printf "Test no overlap";
  Printf.printf "\n";
  let jobs = [
  { start_time = 570; duration = 60;priority = 3};
  { start_time = 660; duration = 45;priority = 5};
  ]in

  let jobs1 = [{start_time = 580; duration = 60; priority = 7};
               {start_time = 550; duration = 100; priority = 8};
               {start_time = 750; duration = 10; priority = 1};]in
  let jobs2 = [{start_time = 20; duration = 60; priority = 7};
               {start_time = 50; duration = 100; priority = 8};
               {start_time = 100; duration = 10; priority = 1};]in
  let tst = schedule_jobs jobs in
  let tst1 = schedule_jobs jobs1 in
  let tst2 = schedule_jobs jobs2 in
  Printf.printf "Testcase 1";
  Printf.printf "\n";
  print_job_list tst;
  Printf.printf "Testcase 2";
  Printf.printf "\n";
  print_job_list tst1;
  Printf.printf "Testcase 3";
  Printf.printf "\n";
  print_job_list tst2;
  
  
  (*Test minimize idle time*)
  let t1 = [
  { start_time = 540; duration = 60; priority = 2 };  
  { start_time = 510; duration = 20; priority = 2 };
  { start_time = 520; duration = 45; priority = 3 };  
  { start_time = 610; duration = 30; priority = 2 };  
  ]in
  Printf.printf "------------------------------------------\n";
  Printf.printf "Test min idle time";
  Printf.printf "\n";
  Printf.printf "Test case 1";
  Printf.printf "\n";
  let min_idle_time = minimize_idle_time t1 in
  print_job_list min_idle_time;

  let t2 = [
  { start_time = 40; duration = 10; priority = 2 };  
  { start_time = 60; duration = 20; priority = 2 };
  { start_time = 100; duration = 15; priority = 3 };  
  { start_time = 90; duration = 10; priority = 2 };  
  ]in

  Printf.printf "Test min idle time";
  Printf.printf "\n";
  Printf.printf "Test case 2";
  Printf.printf "\n";
  let min_idle_time1 = minimize_idle_time t2 in
  print_job_list min_idle_time1;

  let t3 = [
  { start_time = 100; duration = 60; priority = 2 };  
  { start_time = 150; duration = 10; priority = 2 };
  { start_time = 170; duration = 15; priority = 3 };  
  { start_time = 190; duration = 10; priority = 2 };
  { start_time = 170; duration = 5; priority = 2 };  
  ]in

  Printf.printf "Test min idle time";
  Printf.printf "\n";
  Printf.printf "Test case 3";
  Printf.printf "\n";
  
  let min_idle_time2 = minimize_idle_time t3 in
  print_job_list min_idle_time2;

  Printf.printf "-------------------------------------------";
  (*Actual main running*)
  (*Prompt the user for input*)
  let job_num = get_job_number() in
  let job_list = read_jobs job_num in
  
  let test = schedule_jobs job_list in (*Test schedule_jobs on user input*)

  print_job_list test;
 
  (* DEBUG: DELETE LATER*)
  (* print_job_list job_list; *)