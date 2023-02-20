// CSC 310, Spring 2023
/*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  February 1, 2022
    Due Date:       February 8, 2022
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #1
    Filename:       warmup.js
    Purpose:        This program defines various functions that manipulate
                    various data types including integers, strings, and arrays
                    in different ways. Some examples of these functions involve
                    combining arrays, generating a sequence of Fibonacci numbers,
                    or finding the most occuring character in a string.
*/

"use strict";

/*
    Function Name:  fib
    Description:    Returns an array containing the first n numbers in the
                    Fibonacci sequence
    Parameters:     int - n: The number of Fibonacci numbers to return
                             Precondition: n is non-negative
    Return Value:   int array - fibSequence: An array containing the first
                                             n Fibonacci numbers 
*/
function fib(n) {
    //Array to store the sequence
    let fibSequence = [];

    for(let i = 1; i <= n; i++) {
        //Adds 0 to the sequence for the first number
        if(i === 1) {
            fibSequence.push(0);
        }//end if
        //Adds 1 to the sequence for the second number
        else if(i === 2) {
            fibSequence.push(1);
        }//end else if
        //Adds the sum of the two numbers that came before to the sequence
        else {
            fibSequence.push(fibSequence[i - 3] + fibSequence[i - 2]);
        }//end else
    }//end for loop

    return fibSequence;
}//end function fib

/*
    Function Name:  isPalindrome
    Description:    Checks if an integer is a palindrome and returns the result
    Parameters:     int - n: Number to check if it is a palindrome
                             Precondition: n is non-negative
    Return Value:   bool - true: if the number is a palindrome
                           false: if the number is not a palindrome
*/
function isPalindrome(n) {
    //Convert the number to a string
    n = String(n);
    //Convert the string to an array to be able perform array methods
    n = n.split('');

    //Compares the string with its reverse form to see if it's a palindrome
    if(n.join() === n.reverse().join()) {
        //Convert the number back to an integer
        n = Number(n);
        return true;
    }//end if
    //Returns false if the number is not a palindrome
    else {
        //Convert the number back to an integer
        n = Number(n);
        return false;
    }//end else
}//end function isPalindrome

/*
    Function Name:  nthMax
    Description:    Finds the nth largest value in an array of integers, with
                    the largest value defined as n = 0
    Parameters:     int - n: specifies which max to return, with n = 0 being the
                             largest value and n = 1 being the second largest
                             value
                             Precondition:  n is non-negative
                    array - a: array to search for the nth max from
    Return Value:   int - the nth largest value in the array if it exists
                    undefined - returned if the nth max does not exist
*/
function nthMax(n, a) {
    //Sorts the array in descending order
    const sortedArray = [...a].sort((a, b) => b - a);

    //Return the nth maximum value from the array
    return sortedArray[n];
}//end function nthMax

/*
    Function Name:  mode
    Description:    Finds the character in a string that occurs with the
                    highest frequency
    Parameters:     string - s: string to search the mode from
                                Precondition: only one character will have the
                                              highest frequency
    Return Value:   string - the character that occurs the most, will be empty
                             if the input string is empty
*/
function mode(s) {
    //Split the string to search from it
    s = s.split('');

    //Search the string for the most common character
    if(s.length > 0) {
        //Sort the string
        s.sort();

        //Variables
        let currentMode = s[0];
        let count = 1;
        let highestCount = 1;
        let currentLetter = s[0];

        for(let i = 1; i < s.length; i++) {
            if(currentLetter === s[i]) {
                //Count up how many times the current letter appears                
                count++;
                //Counts the number of times the current mode appears
                if(count > highestCount) {
                    highestCount = count;
                    currentMode = s[i];
                }//end if
            }//end if
            else {
                currentLetter = s[i];
                count = 1;
            }//end else
        }//end for
        //Convert the array back to a string
        s = s.join('');
        return currentMode;
    }//end if
    //Return an empty string if the input string is empty
    else {
        //Convert the array back to a string
        s = s.join('');
        return "";
    }//end else
}//end function mode

/*
    Function Name:  combine
    Description:    Combines two arrays into one array where each element is an
                    array containing the element in each array at the same index
                    if the two arrays are the same length
    Parameters:     array - arr1: the first array to be combined
                    array - arr2: the second array to be combined
    Return Value:   array - combinedArray: the array that combines the first and
                                           second array
                    undefined: returned if the arrays are not the same length
*/
function combine(arr1, arr2) {
    let combinedArray = [];

    //Returns undefined if the arrays are not of the same length
    if(arr1.length !== arr2.length) {
        return undefined;
    }//end if
    //Combines the two input arrays
    else {
        for(let i = 0; i < arr1.length; i++) {
            //Sets the element of the combined array to be an array of the elements
            //of the two arrays at the same index
            combinedArray[i] = [arr1[i], arr2[i]];
        }//end for
    }//end else

    return combinedArray;
}//end function combine

/*
    Function Name:  flatten
    Description:    Removes the nesting of subarrays in an array
    Parameters:     array - arr: array to be flattened
    Return Value:   array - arr: the array after it has been flattened
*/
function flatten(arr) {
    //Check if the input is an array
    if(Array.isArray(arr)) {
        //Flattens when the input is an array
        arr = arr.flat();

        //Loop through the array to search for any subarrays in the array 
        for(let i = 0; i < arr.length; i++) {
            arr = arr.flat();
            //Recursively call the function until the element being passed is
            //not an array
            flatten(arr[i]);
        }//end for loop
    }//end if

    return arr;
}//end function flatten

module.exports = { fib, isPalindrome, nthMax, mode, combine, flatten };
