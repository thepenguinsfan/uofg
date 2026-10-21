## General Problem 
The Portfolio program allows users to buy and sell investments with values given by the user. 
Users then have the options to search for investments, update the prices of investments, as well as get the total gain of all investments

## Assumptions
1. User provides input through the given textfields on the GUI
2. User somewhat has knowledge of invesment operations and terms

## Limitations
1. Program only supports 2 types of investments: Stocks and Mutual Funds
3. User must enter a filename for the investments to load and save 

## Building and Running the Program
1. Compile the program: javac eportfolio/Portfolio.java
2. Running the program: java eportfolio/Portfolio "filename"
3. Follow on-screen prompts to navigate the program
## Testing

### Testing for correctness
1. User input has been tested to ensure the program only takes the proper values
2. Test edge cases such as no input, incorrect input, or input that does not logically make sense. Example: Selling negative amount of shares

### Test Cases 
- Input is assumed to be put in the corresponding textfields. Example: AAPL is typed into symbol field when buying/selling/searching invesments, etc.
-Expected output will print to the messages area on the GUI
1. **Buying a New Stock/Mutual Fund**
    - Input: Select Buy from menubar, Select Stock/MutualFund in combo box, `AAPL`, `Apple Inc.`, `10`, `150.00`, Click "Buy" button
    - Expected Output: You have successfully bought 10 shares of Apple Inc. at $150 per share.
    - Invalid Input will result in error messages being printed to the messages area, and will not let user purchase an investment if there is invalid input

2. **Buying an existing Stock/Mutual Fund**
    - Input: Select Buy from menubar, Select Stock/MutualFund in combo box, `AAPL`, `Apple Inc.`, `10`, `150.00`, Click "Buy" button
    - Output: You have successfully bought 10 additional shares of Apple Inc. at $150 per share.
    - Invalid Input will result in error messages being printed to the messages area, and will not let user purchase an investment if there is invalid input
3. **Selling an Existing Stock/Mutual Fund**
    - Input: Select Sell from menubar,  `AAPL`, `10`, `150.00`, Click "Sell" button
    - Expected Output: You have sold 5 shares of Apple for $640.01 with a loss of $-114.98
    - If there are 0 shares of the investment, it will be removed from the list
    - Invalid Input will result in error messages being printed to the messages area, and will not let user sell an investment if there is invalid input
4. **Updating Prices**
    - Input: Select Update from menubar, `xxx.xx`, Click Save
    - Output: The investments' new price is `xxx.xx`
    - Invalid Input will result in error messages being printed to the messages area, and will not let user update the price of an investment if there is invalid input
    -Prev and Next buttons will cycle through the investment list
5. **Calculating Gain**
    - Input: Select Gain from menubar
    - Expected Output: Total gain: $xxx.xx, individual gains will be displayed in messages area
6. **Searching for Investments**
    **Searching with just price range**
    - Input: Select search from menubar, ``, ``, `150`,`100`
    - Output: AAPL, Apple Inc., 5, 130.00, xxx.xx 
    **Searching with a minimum price**
    - Input: Select search from menubar, ``, ``, `100`, ``
    - Output: Print all investments with a price equal to and over 100
    **Searching with a maximum price**
    - Input: Select search from menubar, ``, ``, ``, `200`
    - Output: Print all investments with a price equal to or under 200
    **Searching with just Symbol**
    - Input: Select search from menubar, `AAPL`, ``, ``,``
    - Output: AAPL, Apple Inc., 5, 130.00, xxx.xx 
    **Searching with just keywords**
    - Input: Select search from menubar, ``, `APPLE`, ``,``
    - Output: AAPL, Apple Inc., 5, 130.00, xxx.xx 
    **Searching with all parameters**
    - Input: Select search from menubar, `AAPL`, `APPLE`, `100`,`150`
    - Output: AAPL, Apple Inc., 5, 130.00, xxx.xx 
    **Searching with no parameters**
    - Input: Select search from menubar, ``, ``, ``,``
    - Output: Print all investments in list
    **Searching with mismatching parameters**
    - Input: Select search from menubar, `APPL`, ``, `20`,`100`
    - Output: No investments found

7. **Exiting the program**
    -Input: Select quit from menubar, click X on frame
    -Output: Exit the program, write all investments to filename given 

## Improvements
- Possible improvements if I had more time
1. Get real-time information regarding investments
2. Improve comments on program