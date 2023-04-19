open Sets
(*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  March 21, 2022
    Due Date:       April 3rd, 2022
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #5
    Filename:       fa.ml
    Purpose:        This program defines various functions that can be
                    performed on a NFA, such as converting an NFA to a DFA,
                    returning the states that can be reached from a set of
                    states in the NFA, and finding the epilson enclosure of
                    a set of states in the NFA.                    
*)


(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) fa = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec aux i l =
    if i < 0 then l else aux (i - 1) (s.[i] :: l)
  in
  aux (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

(*
    Function Name:  transition
    Description:    Produces a list of states that the NFA could be in after 
                    one inputted transition starting from the inputted list of states
    Parameters:     nfa: ('q,'s) fa -  the NFA to search from
                    qs: 'q list - list of states to start the transitions from
    Return Value:   'q list - the list of states the NFA could be in after one
                              inputted transition from the given initial states
                     [] - if there are no transitions out of initial states
*)
let transition (nfa: ('q,'s) fa) (qs: 'q list) (s: 's option) : 'q list =
    List.fold_left (fun acc1 x -> union
        (List.fold_left (fun acc2 endState ->
            if elem (x, s, endState) nfa.delta then endState::acc2 else acc2) [] nfa.qs) acc1) 
        [] qs

(*
    Function Name:  e_closure
    Description:    Returns a list of states in the NFA that the NFA
                    might be in after zero or more epsilon transitions
                    on the states in the inputted list
    Parameters:     nfa: ('q,'s) fa - the NFA to search for epsilon
                        transitions from the initial states
                    qs: 'q list - the list of initial states to look
                        for epsilon transitions from
    Return Value:   'q list - the list of states the NFA might be in
                        after zero or more epsilon transitions on the
                        states inputted
*)
let e_closure (nfa: ('q,'s) fa) (qs: 'q list) : 'q list =  

    let rec helper r r' =
        let eTrans = (List.fold_left (fun acc x ->
            if transition nfa [x] None <> [] then
                (transition nfa [x] None)@acc
            else acc)
            [] r) in
        if eq r (union r eTrans) then r else
        helper r' (union r eTrans)
    in helper qs qs

(*
    Function Name:  accept
    Description:    Checks if a string is accepted by an NFA
    Parameters:     nfa: ('q,char) fa - the NFA to check from
                    s: string - the string to test if it is accepted by the NFA
    Return Value:   bool: true if the string is accepted by the NFA
                          false if the string is not accepted by the NFA
*)
let accept (nfa: ('q,char) fa) (s: string) : bool =
    let charList = explode s in
    let rec helper charList currentState =
        match charList with
        | [] ->
            List.fold_left(fun acc x -> if elem x currentState then true else acc) false nfa.fs
        | (h::t) ->
          let newState = e_closure nfa (transition nfa currentState (Some h)) in
            if newState = [] then false
            else helper t newState
    in helper charList (e_closure nfa [nfa.q0])


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(*
    Function Name:  new_states
    Description:    Produces all the DFA states that can be reached after a
                    transition from the list of states. 
    Parameters:     nfa: ('q,'s) fa - the NFA to search the states from
                    qs: 'q list - the list of states to search the new states
                                  from
    Return Value:   'q list list - the list of states that can be reached after
                        a transition out of the states inputed on the NFA
*)
let new_states (nfa: ('q,'s) fa) (qs: 'q list) : 'q list list =
    List.map (fun option -> 
        List.fold_left (fun acc elem -> 
            (e_closure nfa (transition nfa [elem] (Some option)))@acc) 
            [] qs) 
    nfa.sigma

(*
    Function Name:  new_trans
    Description:    Produces all the transitions coming from the list of states
    Parameters:     nfa: ('q,'s) fa - the NFA to search the transitions from
                    qs: 'q list - the list of states to look for transitions from
    Return Value:   ('q list, 's) transition list - the list of transitions
                        coming from the list of states in the NFA, includes
                        the dead state
*)
let new_trans (nfa: ('q,'s) fa) (qs: 'q list) : ('q list, 's) transition list =
    List.fold_left (fun acc option -> 
        (qs, Some option, 
        (List.fold_left (fun acc elem -> 
            (e_closure nfa (transition nfa [elem] (Some option)))@acc) 
            [] qs))::acc) 
    [] nfa.sigma
(*
    Function Name:  new_finals
    Description:    Returns a list of states in an NFA and returns a list of
                    the list if any of the states are a final state in the NFA
    Parameters:     nfa: ('q,'s) fa - the NFA to check final states from
                    qs: 'q list - the list of states to check for final states
                                  from
    Return Value:   'q list list - a list of the input states list when any of
                                   the states are a final state in the NFA
                                 - [] when none of the states are a final state
                                   in the NFA 
*)
let new_finals (nfa: ('q,'s) fa) (qs: 'q list) : 'q list list =
    List.fold_left (fun acc element ->
        if (elem element nfa.fs) = true then [qs] else acc) [] qs 

(*
    Function Name:  getWorkList
    Description:    Helper function to get the list of states in the DFA that
                    have not yet been visited for the nfa_to_dfa_step function
    Parameters:     workList: 'q list list - the current work list
                    dfa: ('q list, 's) fa - the current DFA 
                    nfa: ('q,'s) fa - the NFA being converted to a DFA
    Return Value:   'q list list - the list of states in the DFA that have not
                    yet been visited 
*)
let getWorkList (workList: 'q list list) (dfa: ('q list, 's) fa) (nfa: ('q,'s) fa) : 'q list list =
    let visitedStates = 
        List.fold_left (fun acc x -> 
            union (new_states nfa x) acc 
            ) [] workList in
    minus visitedStates dfa.qs 

(*
    Function Name:  nfa_to_dfa_step
    Description:    Performs the recursion required to convert an NFA to a DFA
                    for the nfa_to_dfa function
    Parameters:     nfa: ('q,'s) fa - the NFA to be a converted to a DFA
                    dfa: ('q list, 's) fa - the current DFA during the
                        conversion, acts as the accumulator
                    work: 'q list list - the current list of states in the
                        DFA that have not been visited
    Return Value:   ('q list, 's) fa - the DFA accumulator 
*)
let rec nfa_to_dfa_step (nfa: ('q,'s) fa) (dfa: ('q list, 's) fa)
    (work: 'q list list) : ('q list, 's) fa =

    match work with
    | [] -> dfa
    | (h::t) ->
        nfa_to_dfa_step nfa (List.fold_left (fun acc x ->
        (let newDFA = {
            sigma = dfa.sigma;
            q0 = dfa.q0;
            qs = union (new_states nfa x) acc.qs;
            fs = union (new_finals nfa x) acc.fs;
            delta = (acc.delta)@(new_trans nfa x)
        } in newDFA)
        ) dfa work) (getWorkList work dfa nfa) 

(*
    Function Name:  nfa_to_dfa
    Description:    Converts an NFA to a DFA, mostly through calling the
                    nfa_to_dfa_step recursive function
    Parameters:     nfa: ('q,'s) fa - the NFA to be converted to a DFA
    Return Value:   ('q list, 's) fa - the DFA the NFA was converted to
*)
let nfa_to_dfa (nfa: ('q,'s) fa) : ('q list, 's) fa =
    let dfa = {
        sigma = nfa.sigma;
        qs = union ((e_closure nfa [nfa.q0])::[]) (new_states nfa (e_closure nfa [nfa.q0]));
        q0 = e_closure nfa [nfa.q0];
        fs = new_finals nfa (e_closure nfa [nfa.q0]);
        delta = new_trans nfa (e_closure nfa [nfa.q0])
    } in 
    nfa_to_dfa_step nfa dfa (new_states nfa (e_closure nfa [nfa.q0]))
