package eportfolio;
import java.util.ArrayList;
import java.util.Scanner;

public class Portfolio {
    private static ArrayList<Stock> stockList = new ArrayList<Stock>();
    private static ArrayList<MutualFund> mutualFundList = new ArrayList<MutualFund>();
    public static void main(String[] args){
        //display menu
        displayMenu();
        //get user choice
        Scanner input = new Scanner(System.in);
        String userChoice = input.nextLine();
        //check if user input is valid
        String choice = isString(userChoice);
        choice = choice.toLowerCase();
        //loop until user exits
        while(!choice.equals("exit") && !choice.equals("e")){
            if(choice.equals("buy") || choice.equals("b")){
                buy();
            }
            else if(choice.equals("sell")){
                sell();
            }
            else if(choice.equals("update") || choice.equals("u")){
                update();
            }
            else if(choice.equals("get gain") || choice.equals("g")){
                getGain();
            }
            else if(choice.equals("search") || choice.equals("search investment")){
                search();
            }
            else if(choice.equals("s")){
                System.out.println("Two methods start with s, please give full name of the method.");
            }
            else{
                System.out.println("Invalid choice. Please enter a valid option.");
            }
            displayMenu();
            userChoice = input.nextLine();
            choice = isString(userChoice);
            choice = choice.toLowerCase();
        }
    } 

    //display menu method
    public static void displayMenu(){
        System.out.println("Please select an option: ");
        System.out.println("Buy");
        System.out.println("Sell");
        System.out.println("Update");
        System.out.println("Get Gain");
        System.out.println("Search Investment");
        System.out.println("Exit");
        System.out.println("Print All(testing)");
    }

    //buy method
    public static void buy(){
        Scanner input = new Scanner(System.in);
        System.out.println("What would you like to buy? (1) Stock (2) Mutual Fund");
        String userInput = input.nextLine();
        //check if user input is valid
        int choice = isInteger(userInput);
        //buy stock
        if(choice == 1){
            System.out.println("Enter the stock symbol: ");
            String userSymbol = input.nextLine();
            String symbol = isString(userSymbol);
            //if the stock is already in the list, buy more
            for(int i = 0; i < stockList.size(); i++){
                if(stockList.get(i).getSymbol().equals(symbol)){
                    System.out.println("Enter the quantity you wish to buy: ");
                    String userQuantity = input.nextLine();
                    //check if user input is valid
                    int quantity = isInteger(userQuantity);
                    System.out.println("Enter the price per share: ");
                    String userPrice = input.nextLine();
                    //check if user input is valid
                    double price = isDouble(userPrice);
                    stockList.get(i).buy(quantity, price);
                    System.out.println("You have successfully bought " + quantity + " additional shares of " + stockList.get(i).getName() + " at $" + price + " per share.");
                    return;
                }
            }
            //if the stock is not in the list, add it
            System.out.println("Enter the name of the stock: ");
            String userName = input.nextLine();
            //check if user input is valid
            String name = isString(userName);
            System.out.println("Enter the quantity you wish to buy: ");
            String userQuantity = input.nextLine();
            //check if user input is valid
            int quantity = isInteger(userQuantity);
            System.out.println("Enter the price per share: ");
            String userPrice = input.nextLine();
            //check if user input is valid
            double price = isDouble(userPrice);
            Stock stock = new Stock(symbol, name, quantity, price);
            stockList.add(stock);
            System.out.println("You have successfully bought " + quantity + " shares of " + name + " at $" + price + " per share.");

        }
        //buy mutual fund
        else if(choice == 2){
            System.out.println("Enter the mutual fund symbol: ");
            String userSymbol = input.nextLine();
            //check if user input is valid
            String symbol = isString(userSymbol);
            //if the mutual fund is already in the list, buy more
            for(int i = 0; i < mutualFundList.size(); i++){
                if(mutualFundList.get(i).getSymbol().equals(symbol)){
                    System.out.println("Enter the quantity you wish to buy: ");
                    String userQuantity = input.nextLine();
                    //check if user input is valid
                    int quantity = isInteger(userQuantity);
                    System.out.println("Enter the price per share: ");
                    String userPrice = input.nextLine();
                    //check if user input is valid
                    double price = isDouble(userPrice);
                    mutualFundList.get(i).buy(quantity, price);
                    System.out.println("You have successfully bought " + quantity + " shares of " + mutualFundList.get(i).getName() + " at $" + price + " per share.");
                    return;
                }
            }
            //if the mutual fund is not in the list, add it
            System.out.println("Enter the name of the mutual fund: ");
            String userName = input.nextLine();
            //check if user input is valid
            String name = isString(userName);
            System.out.println("Enter the quantity you wish to buy: ");
            String userQuantity = input.nextLine();
            //check if user input is valid
            int quantity = isInteger(userQuantity);
            System.out.println("Enter the price per share: ");
            String userPrice = input.nextLine();
            //check if user input is valid
            double price = isDouble(userPrice);
            MutualFund mutualFund = new MutualFund(symbol, name, quantity, price);
            mutualFundList.add(mutualFund);
            System.out.println("You have successfully bought " + quantity + " shares of " + name + " at $" + price + " per share.");
        }
        else{
            System.out.println("Invalid choice. Please enter 1 or 2.");
        }
    }

    //sell method
    public static void sell(){
        Scanner input = new Scanner(System.in);
        System.out.println("What would you like to sell? (1) Stock (2) Mutual Fund");
        String userInput = input.nextLine();
        //check if user input is valid
        int choice = isInteger(userInput);
        //sell stock
        if(choice == 1){
            if(stockList.isEmpty()){
                System.out.println("There are no stocks in the list. Please add a stock.");
                return;
            }
            System.out.println("Enter the stock symbol: ");
            String userSymbol = input.nextLine();
            //check if user input is valid
            String symbol = isString(userSymbol);
            //if the stock is in the list, sell it
            for(int i = 0; i < stockList.size(); i++){
                if(stockList.get(i).getSymbol().equals(symbol)){
                    System.out.println("Enter the quantity you wish to sell: ");
                    String userQuantity = input.nextLine();
                    //check if user input is valid
                    int quantity = isInteger(userQuantity);
                    //if the quantity to sell is greater than the quantity owned, do not sell
                    if(quantity > stockList.get(i).getQuantity()){
                        System.out.println("You do not have enough shares to sell.");
                        return;
                    }
                    System.out.println("Enter the price per share: ");
                    String userPrice = input.nextLine();
                    //check if user input is valid
                    double price = isDouble(userPrice);
                    stockList.get(i).sell(quantity, price);
                    //if the quantity is 0, remove the stock from the list
                    if(stockList.get(i).getQuantity() == 0){
                        stockList.remove(i);
                    }
                    return;
                }
            }
            System.out.println("Stock not found.");
        }
        //sell mutual fund
        else if(choice == 2){
            if(stockList.isEmpty()){
                System.out.println("There are no stocks in the list. Please add a stock.");
                return;
            }
            System.out.println("Enter the mutual fund symbol: ");
            String userSymbol = input.nextLine();
            //check if user input is valid
            String symbol = isString(userSymbol);
            //if the mutual fund is in the list, sell it
            for(int i = 0; i < mutualFundList.size(); i++){
                if(mutualFundList.get(i).getSymbol().equals(symbol)){
                    System.out.println("Enter the quantity you wish to sell: ");
                    String userQuantity = input.nextLine();
                    //check if user input is valid
                    int quantity = isInteger(userQuantity);
                    //if the quantity to sell is greater than the quantity owned, do not sell
                    if(quantity > mutualFundList.get(i).getQuantity()){
                        System.out.println("You do not have enough shares to sell.");
                        return;
                    }
                    System.out.println("Enter the price per share: ");
                    String userPrice = input.nextLine();
                    //check if user input is valid
                    double price = isDouble(userPrice);
                    mutualFundList.get(i).sell(quantity, price);
                    //if the quantity is 0, remove the mutual fund from the list
                    if(mutualFundList.get(i).getQuantity() == 0){
                        mutualFundList.remove(i);
                    }
                    return;
                }   
            }
            System.out.println("Mutual fund not found.");
        }
        else{
            System.out.println("Invalid choice. Please enter 1 or 2.");
        }
    }

    //update price method
    public static void update(){
        Scanner input = new Scanner(System.in);
        //update stock prices
        for(int i = 0; i < stockList.size(); i++){
            System.out.println("Enter the new price for " + stockList.get(i).getName() + ": ");
            String userPricePrice = input.nextLine();
            //check if user input is valid
            double newPrice = isDouble(userPricePrice);
            stockList.get(i).updatePrice(newPrice);
        }
        //update mutual fund prices
        for(int i = 0; i < mutualFundList.size(); i++){
            System.out.println("Enter the new price for " + mutualFundList.get(i).getName() + ": ");
            String userPricePrice = input.nextLine();
            //check if user input is valid
            double newPrice = isDouble(userPricePrice);
            mutualFundList.get(i).updatePrice(newPrice);
        }
    }

    //get gain method
    public static void getGain(){
        double totalGain = 0;
        //calculate gain for stocks
        for(int i = 0; i < stockList.size(); i++){
            totalGain += stockList.get(i).getGain();
        }
        //calculate gain for mutual funds
        for(int i = 0; i < mutualFundList.size(); i++){
            totalGain += mutualFundList.get(i).getGain();
        }
        String totalGainString = String.format("%.2f", totalGain);
        System.out.println("Total gain: $" + totalGainString);
    }

    //search method
    public static void search(){
        double maxPrice = 0;
        double minPrice = 0;
        Scanner input = new Scanner(System.in);
        //get symbol to search for
        System.out.println("Enter the symbol of the investment you would like to search for: ");
        String symbol = input.nextLine().trim();
        symbol = symbol.toLowerCase();
        //get keywords to search for
        System.out.println("Enter any keywords you would like to search for: ");
        String keyWords = input.nextLine().trim();
        //get price range to search for
        System.out.println("Enter the price range you would like to search for: ");
        String priceRange = input.nextLine().trim();

        //set price range
        if(!priceRange.isEmpty()){
            //if a range is given
            if(priceRange.contains("-")){
                String[] range = priceRange.split("-");
                //if 2 numbers are given for a range
                if(!range[0].isEmpty() && !range[1].isEmpty()){
                    minPrice = Double.parseDouble(range[0]);
                    maxPrice = Double.parseDouble(range[1]);
                }
                //if only a max range is given
                else if(range[0].isEmpty() && !range[1].isEmpty()){
                    maxPrice = Double.parseDouble(range[1]);
                }
                //if only a min range is given
                else if(!range[0].isEmpty() && range[1].isEmpty()){
                    minPrice = Double.parseDouble(range[0]);
                    maxPrice = Double.MAX_VALUE;
                }
            }
            //if no range is given
            else{
                minPrice = Double.parseDouble(priceRange);
                maxPrice = Double.parseDouble(priceRange);
            }
        }
        //if all fields are left empty then display all investments
        if(symbol.isEmpty() && keyWords.isEmpty() && priceRange.isEmpty()){
            for (int i = 0; i < stockList.size(); i++){
                System.out.println(stockList.get(i).toString());
            }
            for (int i = 0; i < mutualFundList.size(); i++){
                System.out.println(mutualFundList.get(i).toString());
            }
            return;
        }
        //make keywords lowercase
        keyWords = keyWords.toLowerCase();
        String keyWordsSplit[] = keyWords.split(" ");
        //search for stocks
        for(int i = 0; i < stockList.size(); i++){
            if ((symbol.isEmpty() || stockList.get(i).getSymbol().toLowerCase().equals(symbol)) &&
                (keyWords.isEmpty() || keyWordSearch(keyWordsSplit, stockList.get(i).getName())) &&
                (priceRange.isEmpty() || (stockList.get(i).getPrice() >= minPrice && stockList.get(i).getPrice() <= maxPrice))){
                System.out.println(stockList.get(i).toString());
            } 
        }
        //search for mutual funds
        for(int i = 0; i < mutualFundList.size(); i++){
            if ((symbol.isEmpty() || mutualFundList.get(i).getSymbol().toLowerCase().equals(symbol)) &&
                (keyWords.isEmpty() || keyWordSearch(keyWordsSplit, mutualFundList.get(i).getName())) &&
                (priceRange.isEmpty() || (mutualFundList.get(i).getPrice() >= minPrice && mutualFundList.get(i).getPrice() <= maxPrice))){
                System.out.println(mutualFundList.get(i).toString());
            } 
        }
    }
    //helper method to search for keywords in a name
    private static boolean keyWordSearch(String keywords[], String name){
        int count = 0;
        String nameLower = name.toLowerCase();
        String nameSplit[] = nameLower.split(" ");
        for(int i = 0; i < keywords.length; i++){
            for(int j = 0; j < nameSplit.length; j++){
                if(keywords[i].equals(nameSplit[j])){
                    count++;
                }
            }
        }
        if(count == keywords.length){
            return true;
        }
        return false;
    }

    //helper method to check if a string is an integer
    public static int isInteger(String value){
        Scanner input = new Scanner(System.in);
        value = value.trim();
        while(!value.matches("\\d+") || value.isEmpty() || Integer.parseInt(value) == 0){
            System.out.println("Invalid input. Please enter a positive integer value.");
            value = input.nextLine().trim();
        }
        return Integer.parseInt(value);
    }

    //helper method to check if a string is a double
    public static double isDouble(String value) {
        Scanner input = new Scanner(System.in);  
        value = value.trim();
        while (value.isEmpty() || !value.matches("\\d+(\\.\\d+)?") || Double.parseDouble(value) == 0) {
            System.out.println("Invalid input. Please enter a positive double value.");
            value = input.nextLine().trim();
        }

        return Double.parseDouble(value);  
    }

    //helper method to check if a string is entered
    public static String isString(String value) {
        Scanner input = new Scanner(System.in);  
        value = value.trim();
        while (value.isEmpty()) {
            System.out.println("Invalid input. Please enter a valid string value.");
            value = input.nextLine().trim();
        }

        return value;  
    }
}

    

