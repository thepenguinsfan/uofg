package eportfolio;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.Set;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
public class Portfolio{
    private static ArrayList<Investment> investmentList = new ArrayList<Investment>();
    private static HashMap<String, List<Integer>> nameIndex = new HashMap<>();
    public static void main(String[] args){
        //get the file path from the command line argument
        String fileName = "";
        //check if file name is provided
        if(args.length == 0){
            System.out.println("No file name provided");
        }
        else{
            fileName = args[0];
            fileName = fileName + ".txt";
            readFromFile(fileName);
        }

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
        //save to file
        writeToFile(fileName);
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
    }

    

    //buy method 
    public static void buy(){
        System.out.println("What would you like to buy? (1) Stock (2) Mutual Fund");
        Scanner input = new Scanner(System.in);
        String userInput = input.nextLine();
        //check if user input is valid
        int choice = isInteger(userInput);
        if(choice == 1){
            System.out.println("Enter the symbol of the stock: ");
            String symbol = input.nextLine();
            symbol = isString(symbol);
            //check if symbol exists in mutualfunds
            for(int i = 0; i < investmentList.size(); i++){
                if (investmentList.get(i).getSymbol().equals(symbol) && investmentList.get(i) instanceof MutualFund){
                    System.out.println("This symbol already exists in the list. Please enter a different symbol.");
                    return;
                }
            }
            //if stock already exists in the list, buy more
            for(int i = 0; i < investmentList.size(); i++){
                if(investmentList.get(i).getSymbol().equals(symbol)){
                    System.out.println("Enter the quantity you wish to buy: ");
                    String userQuantity = input.nextLine();
                    int quantity = isInteger(userQuantity);
                    System.out.println("Enter the price per share: ");
                    String userPrice = input.nextLine();
                    double price = isDouble(userPrice);
                    Stock stock = (Stock) investmentList.get(i);
                    stock.buy(quantity, price);
                    System.out.println("You have successfully bought " + quantity + " additional shares of " + investmentList.get(i).getName() + " at $" + price + " per share.");
                    return;
                }
            }
            //if stock is not in the list, add it
            System.out.println("Enter the name of the stock: ");
            String name = input.nextLine();
            name = isString(name);
            System.out.println("Enter the quantity you wish to buy: ");
            String userQuantity = input.nextLine();
            int quantity = isInteger(userQuantity);
            System.out.println("Enter the price per share: ");
            String userPrice = input.nextLine();
            double price = isDouble(userPrice);
            Stock stock = new Stock(symbol, name, quantity, price);
            addInvestment(stock);
            System.out.println("You have successfully bought " + quantity + " shares of " + name + " at $" + price + " per share.");
            
        }
        else if(choice == 2){
            System.out.println("Enter the symbol of the mutual fund: ");
            String symbol = input.nextLine();
            symbol = isString(symbol);
            //check if symbol exists in stocks
            for(int i = 0; i < investmentList.size(); i++){
                if (investmentList.get(i).getSymbol().equals(symbol) && investmentList.get(i) instanceof Stock){
                    System.out.println("This symbol already exists in the list. Please enter a different symbol.");
                    return;
                }
            }
            //if mutual fund already exists in the list, buy more
            for(int i = 0; i < investmentList.size(); i++){
                if(investmentList.get(i).getSymbol().equals(symbol)){
                    System.out.println("Enter the quantity you wish to buy: ");
                    String userQuantity = input.nextLine();
                    int quantity = isInteger(userQuantity);
                    System.out.println("Enter the price per share: ");
                    String userPrice = input.nextLine();
                    double price = isDouble(userPrice);
                    MutualFund mutualFund = (MutualFund) investmentList.get(i);
                    mutualFund.buy(quantity, price);
                    System.out.println("You have successfully bought " + quantity + " additional shares of " + investmentList.get(i).getName() + " at $" + price + " per share.");
                    return;
                }
            }
            //if mutual fund is not in the list, add it
            System.out.println("Enter the name of the mutual fund: ");
            String name = input.nextLine();
            name = isString(name);
            System.out.println("Enter the quantity you wish to buy: ");
            String userQuantity = input.nextLine();
            int quantity = isInteger(userQuantity);
            System.out.println("Enter the price per share: ");
            String userPrice = input.nextLine();
            double price = isDouble(userPrice);
            MutualFund mutualFund = new MutualFund(symbol, name, quantity, price);
            addInvestment(mutualFund);
            System.out.println("You have successfully bought " + quantity + " shares of " + name + " at $" + price + " per share.");
        }
        else{
            System.out.println("Invalid choice. Please enter 1 or 2.");
        }
    }


    //sell method
    public static void sell(){
        Scanner input = new Scanner(System.in);
        System.out.println("Enter the symbol of the investment you wish to sell: ");
        String symbol = input.nextLine();
        symbol = isString(symbol);
        //check if investment exists in the list
        for(int i = 0; i < investmentList.size(); i++){
            if(investmentList.get(i).getSymbol().equals(symbol)){
                System.out.println("Enter the quantity you wish to sell: ");
                String userQuantity = input.nextLine();
                int quantity = isInteger(userQuantity);
                if(quantity > investmentList.get(i).getQuantity()){
                    System.out.println("You do not have enough shares to sell.");
                    return;
                }
                System.out.println("Enter the price per share: ");
                String userPrice = input.nextLine();
                double price = isDouble(userPrice);
                if(investmentList.get(i) instanceof Stock){
                    Stock stock = (Stock) investmentList.get(i);
                    stock.sell(quantity, price);
                    //delete investment if quantity is 0
                    if(stock.getQuantity() == 0){
                        deleteInvestment(i);
                    }
                    return;
                }
                else if(investmentList.get(i) instanceof MutualFund){
                    MutualFund mutualFund = (MutualFund) investmentList.get(i);
                    mutualFund.sell(quantity, price);
                    //delete investment if quantity is 0
                    if(mutualFund.getQuantity() == 0){
                        deleteInvestment(i);
                    }
                    return;
                }
            }
        }
        System.out.println("Investment not found.");
    }


    //update method
    public static void update(){
        Scanner input = new Scanner(System.in);
        for(int i = 0; i < investmentList.size(); i++){
            System.out.println("Enter the new price for " + investmentList.get(i).getName() + ": ");
            String userPrice = input.nextLine();
            double price = isDouble(userPrice);
            investmentList.get(i).updatePrice(price);
        }
    }

    //get gain method
    public static void getGain(){
        double totalGain = 0;
        for(int i = 0; i < investmentList.size(); i++){
            if(investmentList.get(i) instanceof Stock){
                Stock stock = (Stock) investmentList.get(i);
                totalGain += stock.getGain();
            }
            else if(investmentList.get(i) instanceof MutualFund){
                MutualFund mutualFund = (MutualFund) investmentList.get(i);
                totalGain += mutualFund.getGain();
            }
        }
        String totalGainString = String.format("%.2f", totalGain);
        System.out.println("Total gain: $" + totalGainString);
    }

    //create index method
    public static void createIndex(){
        for(int i = 0; i < investmentList.size(); i++){
            //split name into keywords
            String keywords[] = investmentList.get(i).getName().toLowerCase().split(" ");
            //add the position of the investment to the map
            for(int j = 0; j < keywords.length; j++){
                nameIndex.computeIfAbsent(keywords[j], k -> new ArrayList<>()).add(i);
            }
        }
        
    }

    //add investment method
    public static void addInvestment(Investment investment){
        //add investment to the list
        investmentList.add(investment);
        //split the name of the investment into keywords
        String keywords[] = investment.getName().toLowerCase().split(" ");
        //add the position of the investment to the map
        for(int j = 0; j < keywords.length; j++){
            nameIndex.computeIfAbsent(keywords[j], k -> new ArrayList<>()).add(investmentList.size() - 1);
        }
    }
    //delete investment method
    public static void deleteInvestment(int position) {
        //remove the investment from the list
        investmentList.remove(position);
        //iterate through each list of positions in the map
        for (List<Integer> positions : nameIndex.values()) {
            //remove the position from the list
            positions.removeIf(pos -> pos == position);
            //decrement the positions greater than the removed position
            for (int i = 0; i < positions.size(); i++) {
                if (positions.get(i) > position) {
                    positions.set(i, positions.get(i) - 1);
                }
            }
        }
        //remove any empty lists from the map
        nameIndex.values().removeIf(List::isEmpty);
    }

    //search method
    public static void search(){
        double maxPrice = 0;
        double minPrice = 0;
        Scanner input = new Scanner(System.in); 
        //get symbol to search for
        System.out.println("Enter the symbol of the investment you wish to search for: ");
        String symbol = input.nextLine().trim().toLowerCase();
        //get keywords to seacrch for
        System.out.println("Enter the keywords you wish to search for: ");
        String keywords = input.nextLine().trim().toLowerCase();
        //get price range
        System.out.println("Enter the price range you would like to search for: ");
        String priceRange = input.nextLine().trim();

        //set price range
        if(!priceRange.isEmpty()){
            //if a range is given
            if (priceRange.contains("-")) {
                String[] range = priceRange.split("-");
        
                //if 2 numbers are given for a range 
                if (range.length == 2 && !range[0].isEmpty() && !range[1].isEmpty()) {
                    minPrice = Double.parseDouble(range[0]);
                    maxPrice = Double.parseDouble(range[1]);
                }
                //if only a max range is given("-x")
                else if (range.length == 2 && range[0].isEmpty() && !range[1].isEmpty()) {
                    maxPrice = Double.parseDouble(range[1]);
                }
                //if only a min range is given("x-")
                else if (range.length == 1) {
                    minPrice = Double.parseDouble(range[0]);
                    maxPrice = Double.POSITIVE_INFINITY;
                      
                }
            }
            //if a constant price is given
            else{
                minPrice = Double.parseDouble(priceRange);
                maxPrice = Double.parseDouble(priceRange);
            }
        }
        //if all fields are empty, print all investments
        if(symbol.isEmpty() && keywords.isEmpty() && priceRange.isEmpty()){
            for(int i = 0; i < investmentList.size(); i++){
                System.out.println(investmentList.get(i).toString());
            }
            return;
        }
        //if only keywords are empty
        if(keywords.isEmpty()){
            for(int i = 0; i < investmentList.size(); i++){
                double price = investmentList.get(i).getPrice();
                if((symbol.isEmpty() || investmentList.get(i).getSymbol().toLowerCase().equals(symbol))
                && (priceRange.isEmpty() || (price>= minPrice && price <= maxPrice))){
                    System.out.println(investmentList.get(i).toString());
                }
            }
        }
        //else search by keywords
        else{
            Set<Integer> resultPositions = keywordSearch(keywords);
            for(int i = 0; i < investmentList.size(); i++){
                double price = investmentList.get(i).getPrice();
                if((symbol.isEmpty() || investmentList.get(i).getSymbol().toLowerCase().equals(symbol))
                && (priceRange.isEmpty() || (price >= minPrice && price <= maxPrice))
                && resultPositions.contains(i)){
                    System.out.println(investmentList.get(i).toString());
                }
            }
        }
        
    }
    //keyword search method
    public static Set<Integer> keywordSearch(String keywords){
        String[] keywordList = keywords.split(" ");
        Set<Integer> resultPositions = new HashSet<>();

        //add the positions of the first keyword to the result
        if (keywordList.length > 0 && nameIndex.containsKey(keywordList[0])) {
            resultPositions.addAll(nameIndex.get(keywordList[0]));
        }

        //intersect the positions of the remaining keywords with the result
        for (int i = 1; i < keywordList.length; i++) {
            if (nameIndex.containsKey(keywordList[i])) {
                resultPositions.retainAll(nameIndex.get(keywordList[i]));
            } else {
                //if a keyword is not found, clear the result
                resultPositions.clear();
                break;
            }
        }
        //return the indices of the investments that contain all the keywords
        return resultPositions;


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


    //file write method
    public static void writeToFile(String filePath){
        File fileName = new File(filePath);
        try{
            FileWriter writer = new FileWriter(fileName);
            //write each investment to the file
            for(int i = 0; i < investmentList.size(); i++){
                //check type of investment
                if(investmentList.get(i) instanceof Stock){
                    writer.write("stock\n");
                    Stock stock = (Stock) investmentList.get(i);
                    writer.write (stock.toSString() + "\n");
                }
                //check type of investment
                else if(investmentList.get(i) instanceof MutualFund){
                    writer.write("mutualfund\n");
                    MutualFund mutualFund = (MutualFund) investmentList.get(i);
                    writer.write(mutualFund.toSString() + "\n");
                }
            }
            writer.close();
        } catch(IOException e){
            System.out.println("Error saving file.");
            return;
        }
    }
    //read file method
    public static void readFromFile(String filePath) {
        File fileName = new File(filePath);
        try {
            //check if the file exists
            if (!fileName.exists()) {
                //if not, create a new file
                fileName.createNewFile();
                System.out.println("File created: " + fileName.getName());
                return; 
            }

            FileReader fileReader = new FileReader(fileName);
            Scanner reader = new Scanner(fileReader);
            String line;
            while (reader.hasNextLine()) {
                line = reader.nextLine().trim();
                //check type of investment
                if (line.equals("stock")) {
                    //add as a stock
                    String symbol = reader.nextLine();
                    String name = reader.nextLine();
                    int quantity = Integer.parseInt(reader.nextLine());
                    double price = Double.parseDouble(reader.nextLine());
                    Stock stock = new Stock(symbol, name, quantity, price);
                    investmentList.add(stock);
                } else if (line.equals("mutualfund")) {
                    //add as a mutual fund
                    String symbol = reader.nextLine();
                    String name = reader.nextLine();
                    int quantity = Integer.parseInt(reader.nextLine());
                    double price = Double.parseDouble(reader.nextLine());
                    MutualFund mutualFund = new MutualFund(symbol, name, quantity, price);
                    investmentList.add(mutualFund);
                }
            }
            reader.close();
            //create index for the investments in the map
            createIndex();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

