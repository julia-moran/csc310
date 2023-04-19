(*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  February 28, 2022
    Due Date:       March 10, 2022
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #4
    Filename:       tree.ml
    Purpose:        This program creates a user-defined binary tree and
                    defines various functions that can be performed on that
                    tree.
*)
(* CSC 310, Spring 2023, Project 4 *)

(* Type definition for a tree. A tree is either a leaf or a node containing a
   element, a left subtree, and a right subtree *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let empty_tree = Leaf

(* Description: Given a binary search tree, return a new binary search tree
   that with the new element inserted while preserve the property that all
   elements less than the element in a given node are in the left subtree and
   all elements greater than the element are in the right subtree. If the
   element already exists in the tree, then it is not inserted.

   Precondition: the argument t satisfies the binary tree properties

   Parameters: x : 'a - the element to insert
               t : 'a tree - the original binary search tree

   Return: a new binary search tree with the new element inserted properly
*)
let rec tree_insert (x : 'a) (t : 'a tree) : 'a tree =
  match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node (y, l, r) when x > y -> Node (y, l, tree_insert x r)
  | Node (y, l, r) when x = y -> t
  | Node (y, l, r) -> Node (y, tree_insert x l, r)


(****************************************************************************)
(* Implement the functions below. *)
(****************************************************************************)

(* Function Name: tree_mem
   Description: return true if x is an element in the tree t.
   Parameters: x: 'a - the element to check if it is in the tree t
               t: 'a tree - the binary search tree to search from
   Return Value: bool - true if the element x is in the tree t
                        false if the element x is not in the tree t
*)
let rec tree_mem (x : 'a) (t : 'a tree) : bool =
    match t with
    | Leaf -> false
    | Node (element, Leaf, Leaf) -> if element = x then true else false
    | Node (element, leftTree, rightTree) when element = x -> true
    | Node (element, leftTree, rightTree) when element > x -> tree_mem x leftTree
    | Node (element, leftTree, rightTree) -> tree_mem x rightTree 

(* 
   Function Name: tree_insert_all
   Description: return a tree that is the same as tree t, but with all the
   elements in list lst added to it. The elements should be added in the order
   they appear in the list with the tree_insert function.
   Parameters: lst: 'a list - the list of elements to add to the tree
               t: 'a tree - the tree to add the elements in lst to
   Return Value: 'a tree - the original tree t with the elements in lst added
                           to it
*)
let rec tree_insert_all (lst : 'a list) (t : 'a tree) : 'a tree =
    match lst with
    | [] -> t
    | (x::[]) -> tree_insert x empty_tree
    | (x::xs) -> tree_insert x (tree_insert_all xs t)                 

(* 
   Function Name: tree_fold
   Description: given the function f that takes three parameters: the value of
   the current node, the value of the accumulator returned by the left subtree,
   and and the vale of the accumulator returned by the right subtree and an
   initial value init, iterate over the tree t using f to return the
   accumulated value.
   Parameters: f: 'b -> 'a -> 'a -> 'a - a function that takes the value of the
                   current node, accumulator returned by the left subtree, and
                   accumulator returned by the right subtree
               init: 'a: the value of the initial value of the accumulators
               t: 'b tree - the tree to iterate over
   Return Value: 'a - the value accumulated from the fold function
*)
let rec tree_fold (f : 'b -> 'a -> 'a -> 'a) (init : 'a) (t : 'b tree) : 'a =
    match t with
    | Leaf -> init
    | Node (element, Leaf, Leaf) -> f element init init
    | Node (element, leftTree, rightTree) ->
        f element (tree_fold f init leftTree) (tree_fold f init rightTree)

(*
   Function Name: tree_size 
   Description: return the number of elements in the tree t. You must use
   tree_fold to implement your solution.
   Parameters: t: 'a tree - the tree to count the elements of
   Return Value: int - the number of elements in the tree t
*)
let rec tree_size (t : 'a tree) : int = 
        tree_fold (fun _ sum1 sum2 -> sum1 + sum2 + 1) 0 t

(* 
   Function Name: tree_height
   Description: return the height of the tree t. The empty tree has a height of
   zero. You must use tree_fold to implement your solution.
   Parameters: t - 'a tree - the tree to calculate the height from
   Return Value: int - the height of the tree t
*)
let rec tree_height (t : 'a tree) : int =
    if tree_size t = 0 then 0 else
    tree_fold (fun _ leftAcc rightAcc -> 
        if leftAcc > rightAcc then 1 + leftAcc
        else 1 + rightAcc) (-1) t
            
(* 
   Function Name: tree_as_list
   Description: return a list that corresponds to an in-order traversal of
   the tree t. You must use tree_fold to implement your solution.
   Parameters: t: 'a tree - the tree to traverse from
   Return Value: 'a list - the list of elements in the tree t in order
*)
let rec tree_as_list (t : 'a tree) : 'a list = 
    tree_fold (fun currentNode leftAcc rightAcc ->
        leftAcc@[currentNode]@rightAcc) [] t

(* 
   Function Name: tree_map
   Description: return a tree where the function f is applied to all the
   elements of the tree t. You must use tree_fold to implement your solution.
   Parameters: f: 'a -> 'b - the function to apply to the nodes of the tree t
               t: 'a tree - the original tree to apply the function to its nodes   
   Return Value: 'b tree - the original tree t with the function f applied to
                           all its nodes
*)
let rec tree_map (f : 'a -> 'b) (t : 'a tree) : 'b tree =
    tree_fold (fun element leftAcc rightAcc ->
        Node (f element, leftAcc, rightAcc)) empty_tree t

(* This is a BONUS problem. Use the type signature to guide your
   implementation:
   ('a -> 'b tree) -> 'a tree -> 'b tree)
 
   Function Name: tree_bind
   Description: Applies the function f to all the elements in the tree t and
   concatenates the results into a single tree
   Parameters: f: 'a -> 'b tree - the tree of functions to apply to all the
                                  elements in the tree t
               t: 'a tree - the tree to apply the function to
   Return Value: 'b tree - the new tree with the results of the function
                           applied to its elements
*)
(* let rec tree_bind (f : 'a -> 'b tree) (t : 'a tree) : 'b tree = *)
(*
    NOTE: This is an unworking second attempt at defining this function. The results of
    the tree of functions result from a map, and returning a single tree is attempted by
    folding on the tree of trees to create a list and then using that list to create a new
    tree. However, the fold results in a type error.
    
    let results = tree_map f t in
    let resultList = tree_as_list results in
    let finalList = 
        (List.fold_left (fun acc result ->
            (tree_as_list acc)@result) [] resultList) in
    tree_insert_all finalList empty_tree
*)
(*
    NOTE: This is an unworking first attempt at defining this function. A map is used
    to map the functions to the input tree and a fold is used to attempt to return the
    results as a single tree and not a tree of trees, but this still returns a tree of
    trees.

    let results = tree_map f t in
    tree_fold (fun element leftAcc rightAcc ->
        tree_insert element empty_tree) empty_tree results
*)
