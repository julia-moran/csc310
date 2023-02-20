/*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  February 1, 2022
    Due Date:       February 8, 2022
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #1
    Filename:       student.test.js
    Purpose:        This program defines various tests on the functions
                    defined in warmup.js and the class defiend in
                    logmessages.js
*/

const { fib, isPalindrome, nthMax, mode, combine, flatten } = require("../warmup.js");
const LogMessages = require("../logmessages.js");

describe("Student Warmup Tests", () => {
    //My fib function tests
    test("test fib beyond three numbers", () => {
        expect(fib(8)).toStrictEqual([0, 1, 1, 2, 3, 5, 8, 13]);
    });
    test("test fib to 100 elements", () => {
        expect(fib(100)).toStrictEqual([0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,32951280099,53316291173,86267571272,139583862445,225851433717,365435296162,591286729879,956722026041,1548008755920,2504730781961,4052739537881,6557470319842,10610209857723,17167680177565,27777890035288,44945570212853,72723460248141,117669030460994,190392490709135,308061521170129,498454011879264,806515533049393,1304969544928657,2111485077978050,3416454622906707,5527939700884757,8944394323791464,14472334024676220,23416728348467684,37889062373143900,61305790721611580,99194853094755490,160500643816367070,259695496911122560,420196140727489660,679891637638612200,1100087778366101900,1779979416004714000,2880067194370816000,4660046610375530000,7540113804746346000,12200160415121877000,19740274219868226000,31940434634990100000,51680708854858330000,83621143489848430000,135301852344706760000,218922995834555200000]);
    });

    //My isPalindrome function tests
    test("test isPalindrome with a palindrome with even length", () => {
        expect(isPalindrome(1221)).toBe(true);
    });

    //My nthMax function tests
    test("test nthMax with one element", () => {
       expect(nthMax(0, [3])).toBe(3); 
    });

    //My mode function tests
    test("test mode with only one unique character", () => {
        expect(mode("ccc")).toBe("c");
    });
    test("test mode with the mode mixed in within the string", () => {
        expect(mode("babbaaca")).toBe("a");
    });

    //My combine function tests
    test("test combine with arrays of booleans", () => {
        expect(combine([false, true, true], [true, false, true])).toStrictEqual([[false, true], [true, false], [true, true]]);
    });
    test("test combine with nested arrays", () => {
        expect(combine([1, ["a", "b"], 2], [3, ["c", "d"], 4])).toStrictEqual([[1, 3], [["a", "b"], ["c", "d"]], [2, 4]]);
    });    

    //My flatten function tests
    test("test flatten with more levels", () => {
        expect(flatten([[[1, 2], 3], [[[[4], 5, 6], 7], [8, 9], 10]])).toStrictEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    });
});


describe("Student LogMessages Tests", () => {

    let lm;

    beforeEach(() => {
        lm = new LogMessages();
    });


    //My add method method tests
    test("test add empty string as message", () => {
        expect(lm.add(1, "E", "")).toBe(false);
    });

    //My listByType method tests
    test("test listByType with multiple unordered matches", () => {
        lm.add(3, "W", "a");
        lm.add(1, "W", "b");
        lm.add(2, "E", "c");
        lm.add(4, "W", "d");
        expect(lm.listByType("W")).toStrictEqual(["W 1 b", "W 3 a", "W 4 d"]);
    });

    //My listByTimeWindow method tests
    test("test listByTimeWindow with no matches", () => {
        lm.add(1, "E", "a");
        lm.add(2, "W", "b");
        expect(lm.listByTimeWindow(4, 5)).toStrictEqual([]);
    }); 
    test("test listByTimeWindow with max value out of range", () => {
        lm.add(3, "I", "a");
        lm.add(1, "E", "b");
        lm.add(2, "W", "c");
        expect(lm.listByTimeWindow(2, 5)).toStrictEqual(["W 2 c", "I 3 a"]);
    });
    test("test listByTimeWindow with min value out of range", () => {
        lm.add(4, "W", "a");
        lm.add(3, "E", "b");
        lm.add(1, "I", "c");
        expect(lm.listByTimeWindow(2, 4)).toStrictEqual(["E 3 b", "W 4 a"]);
    });
    test("test listByTimeWindow with low equal to high", () => {
        lm.add(4, "W", "a");
        lm.add(1, "I", "c");
        expect(lm.listByTimeWindow(4, 4)).toStrictEqual(["W 4 a"]);
    });

});
