// CSC 310, Spring 2023
/*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  February 8, 2023
    Due Date:       February 15, 2023
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #2
    Filename:       regexp.js
    Purpose:        This program defines various functions that match or replace
                    input strings using regular expressions.
*/

"use strict";

// Function Name: matchFloats
// Description: match floating point numbers in a string and return an array
// of all matches
// Signature: String -> Array[String]
// Example: "0.23"
// Parameters: str - string: input string to be matched
// Return Value: matchedStr - array: array of floating point numbers in the
//                                   array
//               [] - array: an empty array if there are no matches
function matchFloats(str) {
    const regexp = /-*(\d*\.\d*)/g;
    const matchedStr = str.match(regexp);
    return matchedStr ? matchedStr : [];
}//end function matchFloats

// Function Name: matchVowelWords
// Description: match words in a string that begin and end with a vowel and
// return an array of all matches
// Signature: String -> Array[String]
// Example: "Orange"
// Parameters: str - string: input string to be matched
// Return Value: matchedStr - array: array of words in the input string that
//                                   begin and end with a vowel
//               [] - array: an empty array if there are no matches
function matchVowelWords(str) {
    const regexp = /(\b[aeiou]\w*[aeiou]\b)|(\b[aeiou]\b)/gi;
    const matchedStr = str.match(regexp);
    return matchedStr ? matchedStr : [];
}

// Function Name: matchISODates
// Description: match ISO dates in a string that begin and return an array
// of all matches. An ISO date has the form YYYY-MM-DD.
// Signature: String -> Array[String]
// Precondition: the date is valid
// Example: "2023-02-15"
// Parameters: str - string: input string to be matched, must be a valid date
// Return Value: matchedStr - array: array of ISO dates
//               [] - array: an empty array if there are no matches
function matchISODates(str) {
    const regexp = /((\d{4})-(\d{2})-(\d{2}))/g;
    const matchedStr = str.match(regexp);
    return matchedStr ? matchedStr : [];
}//end function matchISODates

// Function Name: matchHexGrayscaleColors
// Description: match 12-bit and 24-bit hex colors in a string that have the
// same value for the red, green and blue values and return an array of all
// matches. A hex color has the form #HHH or #HHHHHH where H is a hexadecimal
// digit.
// Signature: String -> Array[String]
// Example: "#848484"
// Parameters: str - string: input string to be matched
// Return Value: matchedStr - array: array of hex colors with the same value for
//                                   the red, blue, and green color values
//               [] - array: an empty array if there are no matches
function matchHexGrayscaleColors(str) {
    const regexp = /#(([0-9]|[A-F])|(([0-9]|[A-F])([0-9]|[A-F])))\1\1(?![A-F0-9])/g;
    const matchedStr = str.match(regexp);
    return matchedStr ? matchedStr : [];
}//end function matchHexGrayscaleColors

// Function Name: matchUMLInterface
// Description: match the syntax for a UML interface in a class diagram. A UML
// interface is delimited by "<<" and ">>".
// Signature: String -> Array[String]
// Example: "<<Interface>>"
// Parameters: str - string: input string to be matched
// Return Value: matchedStr - array: array of strings surrounded by "<<" and ">>"
//               [] - array: an empty array if there are no matches
function matchUMLInterface(str) {
    const regexp = /(<{2}.+?>{2})/g;
    const matchedStr = str.match(regexp);
    return matchedStr ? matchedStr : [];
}//end function matchUMLInterface

// Function Name: replaceRepeatedWords
// Description: replace repeated words in a string with a single instance of
// the repeated string.
// Signature: String -> String
// Example: "the the" -> "the"
// Parameters: str - string: input string to be replaced
// Return Value: string: inputted string replaced with one without repeated
//                       consecutive words
function replaceRepeatedWords(str) {
    const regexp = /((\b\w+\b\s+)|(\s+\b\w+\b))\1+/g;
//    const regexp = /(((\b\w+\b)(?=\s+))|((\b\w+\b)(?<=\s+)))\1+/g;
    const replacement = "$1";
    return str.replace(regexp, replacement);
}//end function replaceRepeatedWords

// Function Name: replacePairContents
// Description: replace the pair content in a string. A pair is specified as
// (<first>,<second>); the replacement pair should have the elements swapped
// to become (<second>,<first>).
// Signature: String -> String
// Example: "(x,y)" -> "(y,x)"
// Parameters: str - string: input string to be replaced
// Return Value: string: inputted string replaced with the elements in each
//                       pair swapped with one another
function replacePairContents(str) {
    const regexp = /\((.+?),(.+?)\)/g;
    const replacement = "($2,$1)";
    return str.replace(regexp, replacement);
}//end function replacePairContent

// Function Name: replaceFloatPrecision
// Description: replace instances of floating point numbers in a string that
// have more than two digits after the decimal point to have only two digits
// after the decimal point.
// Signature: String -> String
// Example: "1.2345" -> "1.23"
// Parameters: str - string: input string to be replaced
// Return Value: string: inputted string replaced with all floating point
//                       numbers having at most two digits after the decimal point
function replaceFloatPrecision(str) {
    const regexp = /(\d*\.\d{2})\d*/g;
    const replacement = "$1";
    return str.replace(regexp, replacement);
}//end function replaceFloatPrecision

// Function Name: replaceIntegerFormat
// Description: replace instances of integers in string with the same integer
// but with underscores before each group of three digits.
// Signature: String -> String
// Example: "123456" -> "123_456"
// Precondition: integers in the string are separated by whitespace.
// Parameters: str - string: input string to be replaced
// Return Value: string: inputted string replaced with an underscore between
//                       each group of three digits
function replaceIntegerFormat(str) {
    const regexp = /(\d)(?=(\d{3})+\b)/g;
    const replacement = "$1_";
    return str.replace(regexp, replacement);
}//end function replaceIntegerFormat

// Function Name: replaceURLQuery
// Description: replace a URL with the query portion of the URL. See
// https://en.wikipedia.org/wiki/URL for more details about the URL format.
// Signature: String -> String
// Example: "http://google.com?q=cats" -> "q=cats"
// Precondition: the string is a valid URL
// Parameters: str - string: input string to be replaced
// Return Value: string: inputted string replaced with only the query portion
//                       of a URL
function replaceURLQuery(str) {
    const regexp = /.*((?<=\?).*)/g;
    const replacement = "$1";
    return str.replace(regexp, replacement);
}//end function replaceURLQuery


module.exports = {
    matchFloats,
    matchVowelWords,
    matchISODates,
    matchHexGrayscaleColors,
    matchUMLInterface,
    replaceRepeatedWords,
    replacePairContents,
    replaceFloatPrecision,
    replaceIntegerFormat,
    replaceURLQuery
};
