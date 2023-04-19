(*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  February 17, 2022
    Due Date:       February 27, 2022
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #3
    Filename:       warmup.ml
    Purpose:        This program defines various functions that operate on
                    integers, tuples, and lists in different ways. Some examples
                    of these functions involve calculating the absolute value
                    of an integer, finding the greatest common divisor between
                    two integers, and merging two sorted lists. Some of these
                    functions involve recursion and/or pattern matching.
*)
(* CSC 310, Spring 2021 *)

(****************************************************************************)
(* Part 1: Non-Recursive Functions *)
(****************************************************************************)

(* Implement the following functions that do not require recursion.
You may add the rec keyword to any of the following functions or write a
recursive helper function. Just remember that if you write a helper function,
it must be defined in the file before it is called. *)

(*
Function Name: abs
Type: int -> int
Description: Returns the absolute value of x.
Parameters: x - int: number inputed
Return Value: int: the absolute value of the inputed number

Examples:
    abs 1 = 1
    abs (-5) = 5
*)
let abs x = 
    if x < 0 then
        (-x)
    else
        x

(*
Function Name: is_odd
Type: int -> bool
Description: Returns whether or not x is odd.
Parameters: x - int: number inputed
Return Value: bool: true if the number inputed is odd
                    false if the number inputed is even

Examples:
  is_odd 1 = true
  is_odd 4 = false
  is_odd (-5) = true
*)
let is_odd x = 
    let modResult = x mod 2 in
    if modResult = 0 then
        false
    else
        true
(*
Function Name: divides
Type: int -> int -> bool
Description: Returns true iff x divides y. Formally, x | y iff x ≠ 0 and there
exists integer k such that y = xk.
Parameters: x - int: the inputed divisor
            y - int: the inputed dividend
Return Value: bool: true if x divides y
                    false if x does not divide y

Examples:
    divides 10 5 = false 
    divides 10 20 = true
    divides 10 10 = true
    divides 10 0 = true
    divides 0 15 = false
    divides 0 0 = false
*)
let divides x y =
    if y mod x = 0 then
        true
    else
        false
(*
Function Name: area
Type: int * int -> int * int -> int
Description: Takes in the Cartesian coordinates (2-dimensional) of any pair of
opposite corners of a rectangle and returns the area of the rectangle. The
sides of the rectangle are parallel to the axes.
Parameters: x - tuple of integers: the coordinates of a corner of a rectangle 
            y - tuple of integers: the coordinates of a corner of a rectangle
                                   opposite of the other corner 
Return Value: int: the area of the rectangle bounded by the two inputed corners                                   

Examples:
  area (1, 1) (2, 2) = 1
  area (2, 2) (1, 1) = 1
  area (2, 1) (1, 2) = 1
  area (0, 1) (2, 3) = 4
  area (1, 1) (1, 1) = 0
  area ((-1), (-1)) (1, 1) = 4
*)
let area x y = 
    let (a, b) = x in
    let (c, d) = y in

    abs((a - c) * (b - d))
(*
Function Name: equiv_frac
Type: (int * int) -> (int * int) -> bool
Description: Takes in two fractions in the form of 2-tuples, a/b and c/d, and
returns whether or not the two fractions are equivalent. In the case of
division by 0, false should be returned. The optimal solution does not
require recursion, but you may add rec keyword or define your own recursive
helper function if you would like.
Parameters: (a, b) - tuple of integers: the first fraction, a being the
                     numerator and b being the denominator
            (c, d) - tuple of integers: the second fraction, c being the
                      numerator and d being the denominator
Return Value: bool: true if the two fractions are equivalent
                    false if the two fractions are not equivalent 

Examples:
    equiv_frac (1, 2) (2, 4) = true
    equiv_frac (2, 4) (1, 2) = true
    equiv_frac (1, 2) (1, 2) = true
    equiv_frac (3, 12) (2, 6) = false
    equiv_frac (1, 2) (3, 4) = false
    equiv_frac (1, 0) (2, 0) = false
*)
let equiv_frac (a, b) (c, d) =
    if b = 0 || d = 0 then
        false
    else if (a * d) = (b * c) then
        true
    else
        false

(****************************************************************************)
(* Part 2: Recursive Functions *)
(****************************************************************************)
(* Implement the following functions with recursion *)

(*
Function Name: gcd
Type: int -> int -> int
Description: Returns the greatest common divisor of a and b. Utilize the
recursive Euclidean Algorithm to accomplish this:
https://en.wikipedia.org/wiki/Euclidean_algorithm
Parameters: a - int: the first inputed number
            b - int: the second inputed number
Return Value: int: the greatest common divisor of the two inputed numbers

Examples:
    gcd 8 12 = 4
    gcd 54 24 = 6
    gcd 10 0 = 10
    gcd 0 10 = 10
    gcd 0 0 = 0 
*)
let rec gcd a b =
    if b = 0 then
        a
    else
        gcd b (a mod b)
(*
Function Name: ack
Type: int -> int -> int
Description: Returns the value of the Ackermann–Péter function for m and n:
https://en.wikipedia.org/wiki/Ackermann_function
Parameters: m - int: first number inputed
            n - int: the second number inputed
Return Value: int: the resulted value of the Ackermann–Péter function for the
                   two inputed numbers

Examples:
  ack 0 0 = 1
  ack 0 5 = 6
  ack 3 0 = 5
  ack 3 3 = 61
*)
let rec ack m n =
    if m = 0 then
        n + 1
    else if n = 0 then
        ack (m-1) 1
    else
        ack (m-1) (ack m (n-1))
(*
Function Name: pow
Type: int -> int -> int
Description: Returns x raised to the power y.
Assumptions: y is non-negative, your code will not be tested for integer
overflow cases.
Parameters: x - int: the number serving as the base of the function
            y - int: the number serving as the exponent of the function
Return Value: int: the value of the base raised to the power of the exponent

Examples:
  pow 3 1 = 3
  pow 3 2 = 9
  pow (-3) 3 = -27
*)
let rec pow x y =
    if y = 1 then
        x
    else if y = 0 then
        1
    else
        x * pow x (y - 1)

(*
Function Name: log
Type: int -> int -> int
Description: Returns the log of y with base x rounded-down to an integer.
Assumptions: You may assume the answer is non-negative, x >= 2, and y >= 1.
Parameters: x - int: the number serving as the base of the function
            y - int: the number serving as the argument for the log function
Return Value: int: the value of the log of the argument with the inputed base
                    rounded down to an integer

Examples:
  log 4 4 = 1
  log 4 16 = 2
  log 4 15 = 1
  log 4 64 = 3
*)
let rec log x y = 
    if y < x then
        0
    else if y = x then
        1
    else
        1 + log x (y / x)
(*
Function Name: len
Type: int -> int
Description: Returns the number of numerical digits in x.
Parameters: x - int: number inputed
Return Value: int: the number of numerical digits in the number inputed

Examples:
    len 1 = 1
    len 10 = 2
    len 15949 = 5
*)
let rec len x =
    if abs(x) < 10 then
        1
    else
        1 + len(abs(x) / 10)

(****************************************************************************)
(* Part 3: Lists *)
(****************************************************************************)

(*
Function Name: getHelper
Type: int -> 'a list -> int -> 'a
Description: Helper function for the get function, returns the element at
the index idx in the list lst. If idx is past the bounds of lst, raise an
error using failwith "Out of bounds".
Assumptions: idx is non-negative.
Parameters: idx - int: the index to get the element from the list from
            lst - list: the list to get the element from
            currentIndex - int: the current index in the search for the correct
                                element at index idx
Return Value: the element at the inputed index of the list
              or an out of bounds error  
*)

let rec getHelper idx lst currentIndex = 
  match lst with
        | [] -> failwith "Out of bounds"
        | (h::t) ->
            if idx < currentIndex then
                failwith "Out of bounds"
            else if idx = currentIndex then
                h
            else
                getHelper idx t (currentIndex + 1)  

(*
Function Name: get
Type: int -> 'a list -> 'a
Description: Returns the element at the index idx in the list lst. If idx is
past the bounds of lst, raise an error using failwith "Out of bounds".
Assumptions: idx is non-negative.
Parameters: idx - int: the index of the element to return from the list
            lst - list: the list to get the element from 
Return Value: the element at the inputed index of the list
              or an out of bounds error

Examples:
  get 0 [26; 11; 99] = 26
  get 1 [26; 11; 99] = 11
  get 2 [26; 11; 99] = 99
  get 3 [26; 11; 99] = Exception
  get 0 ["a"; "b"] = "a"
  get 1 ["a"; "b"] = "b"
  get 2 ["a"; "b"] = Exception
*)
let rec get idx lst =
    getHelper idx lst 0

(*
Function Name: partialSumHelper
Type: int -> int lst -> int -> int -> int
Description:   Helper function for the partial_sum function. Returns the sum of
               the elements from index 0 to the inputed index, or returns the
               sum of all the elements in the list if the inputed index is 
               beyond the upper bound of the list
Parameters:    idx - int: the index to stop the count of the sum at
               lst - list: the list to sum up the elements from
               sum - int: the current sum of the elements in the list
               currentIndex - int: keeps track of the current index in the list
Return Value:  int: the sum of the elements up to the inputed index
*)
(*
Type: int -> int list -> int

Description: Returns the sum of the values from index 0 to index idx
(0-indexed) of lst inclusive. If idx is greater than greatest index of lst,
return the sum of all elements of lst. You may assume idx is non-negative, and
your implementation does not have to consider this case.

Examples:
    partial_sum 0 [5;6;7;3] = 5
    partial_sum 1 [5;6;7;3] = 11
    partial_sum 4 [5;6;7;3] = 21
    partial_sum 2 [] = 0
*)
let rec partialSumHelper idx lst sum currentIndex = match lst with
    | [] -> sum
    | (h::t) ->
        if currentIndex = idx then
            sum + h
        else
            partialSumHelper idx t (sum + h) (currentIndex + 1)
(*
Function Name: partial_sum
Type: int -> int list -> int
Description: Returns the sum of the values from index 0 to index idx
(0-indexed) of lst inclusive. If idx is greater than greatest index of lst,
return the sum of all elements of lst. You may assume idx is non-negative, and
your implementation does not have to consider this case.
Parameters:    idx - int: the index to stop the count of the sum at
               lst - list: the list to sum up the elements from
Return Value:  int: the sum of the elements up to the inputed index

Examples:
    partial_sum 0 [5;6;7;3] = 5
    partial_sum 1 [5;6;7;3] = 11
    partial_sum 4 [5;6;7;3] = 21
    partial_sum 2 [] = 0
*)
let rec partial_sum idx lst =
    partialSumHelper idx lst 0 0

(*
Function Name: partial_sums
Type: int list -> int list -> int list
Description: Returns a list where the values correspond to the partial sums
from 0 to each index of lst listed in xs. Follows the same rules as partial_sum
for special cases.
Parameters: xs - int list: determines the indexes of the list to include in the
                           partial sums
            lst - int list: the list to calculate the partial sums from
Return Value: int list: a list containing the sums of the selected indexes from
                        the list

Examples:
    partial_sums [0;1] [5;6;7;3] = [5;11] 
    partial_sums [1;4] [5;6;7;3] = [11;21]
    partial_sums [] [5;6;7;3] = []
    partial_sums [0;1] [] = [0;0]
*)
let rec partial_sums xs lst = match xs with
    | [] -> []
    | (h1::t1) ->
        (partial_sum h1 lst::partial_sums t1 lst)
    
(*
Function Name: zip
Type: 'a list -> 'b list -> ('a * 'b) list
Description: Given two lists, returns list of pairs where elements at the same
index are paired up in the same order. If one of the lists runs out of
elements, then do not construct any more pairs.
Parameters: lst1 - list: the first list to combine
            lst2 - list: the second list to combine
Return Value: list of tuples: a list in which the elements at the same index
                              are paired up in a tuple

Examples:
    zip [1;3] [2;4] = [(1,2);(3,4)] 
    zip [1;3] [2;4;5] = [(1,2);(3,4)] 
    zip [] [5;4;3] = []
    zip [] [] = []
*)
let rec zip lst1 lst2 = match (lst1, lst2) with
    | ([], h::t) -> []
    | (h::t, []) -> []
    | ([], []) -> []
    | (h1::t1, h2::t2) -> 
        (h1, h2)::(zip t1 t2)

(*
Function Name: merge
Type: 'a list -> 'a list -> 'a list
Description: Merge two sorted lists, lst1 and lst2, and return the result as a
sorted list.
Parameters: lst1 - list: the first sorted list to merge
            lst2 - list: the second sorted list to merge
Return Value: list: a list with the two inputed lists merged into one sorted
                    list

Examples:
  merge [1] [2] = [1;2]
  merge [] [] = []
  merge [1; 4] [2; 3] = [1; 2; 3; 4]
  merge [1] [0] = [0; 1]
*)
let rec merge lst1 lst2 = match (lst1, lst2) with
    | ([], _) -> lst2
    | (_, []) -> lst1
    | (h1::t1, h2::t2) ->
        if h1 < h2 then
            h1::merge t1 lst2
        else if h1 > h2 then
            h2::merge lst1 t2  
        else
            h1::merge t1 lst2

(*
This is a BONUS problem.
Function Name: shift_left
Type: int -> 'a list -> 'a list
Description: Move every element over in lst to the left by the given shift
(looping around).
Assumptions: shift is non-negative.
Parameters: shift - int: the index to shift the list by
            lst - list: the list to be shifted
Return Value: list: the list after having been shifted to the left as many
                    times as specified by the shift parameter 

Note: There are no tests for this function; write your own tests to
verify that your behavior is what you would expect.

Examples:
  shift_left 0 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"]
  shift_left 1 ["a"; "b"; "c"; "d"] = ["b"; "c"; "d"; "a"]
  shift_left 2 ["a"; "b"; "c"; "d"] = ["c"; "d"; "a"; "b"]
  shift_left 3 ["a"; "b"; "c"; "d"] = ["d"; "a"; "b"; "c"]
  shift_left 4 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"]
  shift_left 5 ["a"; "b"; "c"; "d"] = ["b"; "c"; "d"; "a"]
*)
let rec shift_left shift lst = match lst with
    | [] -> []
    | (h::t) -> 
        if shift = 0 then
            lst
        else
            shift_left (shift - 1) (t@h::[])
