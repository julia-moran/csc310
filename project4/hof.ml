(*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  February 28, 2022
    Due Date:       March 10, 2022
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #4
    Filename:       hof.ml
    Purpose:        This program defines various functions that can be
                    performed on a list. All use at least one instance
                    of the methods List.map, List.fold_left, or
                    List.fold_right to accomplish the tasks.
*)
(* CSC 310, Spring 2023, Project 4 *)

(* Instructions: You must use List.map, List.fold_left, or List.fold_right to
   complete these functions. You may not use any other functions in the List
   module. If the rec keyword is added to a function, it will be considered
   incorrect. Note: Some of these functions may require a combination of
   mapping and folding. *)


(* 
   Function Name: len
   Description: return the length of lst
   Parameters: lst: 'a list - the arbitrary list to get the length from
   Return Value: int - the length of the list lst
*)
let len (lst : 'a list) : int = 
    List.fold_left (fun sum _ -> sum + 1) 0 lst


(* 
   Function Name: elem_count
   Description: return the number of elements in lst that are equal to target
   Parameters: lst: 'a list - the list to search for the number of elements
                              equal to the target from
               target: 'a - the value to count how many times it occurs in the
                            list
    Return Value: int - the number of times the target occurs in the list 
*)
let elem_count (lst : 'a list) (target : 'a) : int = 
    List.fold_left (fun sum h -> if h = target then sum + 1 else sum) 0 lst
    

(* 
   Function Name: uniq
   Description: return a list with all duplicate elements removed. The order of
   the resulting list does not matter.
   Parameters: lst: 'a list - the list to remove the duplicate elements removed
   Return Value: 'a list - the original list lst with any elements that occur
                           more than once removed from the list
*)
let uniq (lst : 'a list) : 'a list =
    List.fold_left (fun acc h -> if (elem_count acc h) = 1 then acc else h::acc) [] lst

(* 
   Function Name: occurrence_list
   Description: return a list of tuples where the first member is an element of
   lst and the second member is the number of occurrences of first tuple
   element in lst. The result should contain no duplicates.
   Parameters: lst: 'a list - the list to get the tuples of element value and 
                              number of times that value occurs in the list from
   Return Value: ('a * int) list - a list containing tuples, the first element of
                  the tuple being the value in the list and the second element of
                  the tuple being the number of times that element occurs in the
                  the list
*)
let occurrence_list (lst : 'a list) : ('a * int) list =
    uniq (List.fold_left (fun acc h -> (h, elem_count lst h)::acc) [] lst)
    
(*
   Function Name: bind
   Description: Apply the function f to all elements in lst and then
   concatenate the results into a single list.
   Parameters: f: 'a -> 'b list - the function returning a list of functions to
                                  apply to the list
               lst: 'a list - the list whose elements the functions will be 
                              applied to
   Return Value: 'b list - a list in which the elements are the result of the
                           function applied to the original list
*)
let bind (f : 'a -> 'b list) (lst : 'a list) : 'b list = (* List.fold_left (List.map (fun apply -> List.map apply lst) f) [] lst *)
    let results = List.map f lst in
    List.fold_left (fun acc result -> acc@result) [] results

(* Function Name: ap
   Description: apply each function in fs to each argument in args in order
   Parameters: fs: ('a -> 'b) list - the list of functions to apply to the list
               args: 'a list - the list whose elements will be parameters of the
                               functions 
   Return Value: 'b list - the new list after the functions have been applied to
                           the original list
*)
let ap (fs : ('a -> 'b) list) (args : 'a list) : 'b list =
    let returns = List.map (fun apply -> List.map apply args) fs in
    List.fold_left (fun acc return -> acc@return) [] returns
