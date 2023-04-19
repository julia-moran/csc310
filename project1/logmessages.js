// CSC 310, Spring 2023
/*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  February 1, 2023
    Due Date:       February 8, 2023
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #1
    Filename:       logmessages.js
    Purpose:        This program defines the LogMessages class, which will
                    store log messages, which contain a type, timestamp, and
                    message.
*/

"use strict";

class LogMessages {

    /*
        Function Name:  constructor
        Description:    Inititalizes the array object and its length
        Parameters:     N/A
        Return Value:   N/A
    */
    constructor() {
        this.length = 0;
        this.logs = [];
    }//end constructor

    /*
        Function Name:  add
        Description:    Attempts to add a new log message
        Parameters:     int - messageTime: the logical timestamp
                        string - messageType: type of message, can be I, W, or E
                        string - messageText: the message text
        Return Value:   boolean - true: if the message was successfully added
                                  false: if the message was not successfully added
    */
    add(messageTime, messageType, messageText) {
        let logs = this.logs;

        //Check if the message text is the empty string
        if(messageText === "") {
            return false;
        }//end if

        //Check if the message text is not I, W, or E
        if((messageType !== "I") && (messageType !== "W") && (messageType !== "E")) {
            return false;
        }//end if

        //Check if the logical timestamp already exists in the array
        for(let i = 0; i < logs.length; i++) {
            if(logs[i][1] === messageTime) {
                return false;
            }//end if
        }//end for log
        

        //Add the log message if all the tests above pass
        this.logs[this.length] = [messageType, messageTime, messageText];
        this.length++;
        return true;
    }//end function add

    /*
        Function Name:  listAll
        Description:    Returns an array of strings representing the log
                        messages added ordered by logical timestamp
        Parameters:     N/A
        Return Value:   array - strings: array of log messages as strings ordered
                                         by logical timestamp       
    */
    listAll() {
        let logs = this.logs;
        //Sort the log messages by timestamp in ascending order
        logs.sort((a, b) => a[1] - b[1]);

        let strings = [];

        for(let i = 0; i < logs.length; i++) {
            //Turn each array in the object array into a string
            strings[i] = logs[i].join(" ");
        }//end for loop

        return strings;
    }//end function listAll

    /*
        Function Name:  listByType
        Description:    Returns an array of strings of a chosen type from the
                        object array ordered by logical timestamp
        Parameters:     string - t: the type of messages to return
        Return Value:   array - logsOfType: array of strings from the object array
                                            of the type chosen sorted by timestamp
    */
    listByType(t) {
        //Variables
        let logs = this.logs;
        let logsOfType = [];
        let typeIndex = 0;

        //Sort the array by logical timestamp
        logs.sort((a, b) => a[1] - b[1]);

        for(let i = 0; i < logs.length; i++) {
            //Checks if the type of message is of the specified type
            if(logs[i][0] === t) {
                //Joins the chosen elements as a string
                logsOfType[typeIndex] = logs[i].join(" ");
                typeIndex++;
            }//end if
        }//end for loop

        return logsOfType;
    }//end function listByType

    /*
        Function Name:  listByTimeWindow
        Description:    Returns array of string from the object array in which the
                        logical timestamps are within the specified range and
                        ordered by timestamp
        Parameters:     int - lo: the minimum value the timestamps chosen can be
                        int - hi: the maximum value the timestamps chosen can be
        Return Value:   array of strings - logsByTime: the array of strings from
                                           the object array within the time range
    */
    listByTimeWindow(lo, hi) {
        //Variables
        let logs = this.logs;
        let logsByTime = [];
        let timeIndex = 0;

        //Sort the log messages by the logical timestamp
        logs.sort((a, b) => a[1] - b[1]);

        for(let i = 0; i < logs.length; i++) {
            //Check if the message timestamp falls within the chosen range
            if((logs[i][1] >= lo) && (logs[i][1] <= hi)) {
                //Join the chosen timestamps as a string
                logsByTime[timeIndex] = logs[i].join(" ");
                timeIndex++;
            }//end if
        }//end for loop

        return logsByTime;
    }//end function listByTimeWindow
}//end class LogMessages

module.exports = LogMessages;
