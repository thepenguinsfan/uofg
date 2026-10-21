package eportfolio;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.HashSet;
public class Portfolio {
    private static ArrayList<Investment> investmentList = new ArrayList<Investment>();
    private static HashMap<String, List<Integer>> nameIndex = new HashMap<>();
    public static void main(String[] args) {
        //get the file path from the command line argument
        String fileName = "";
        boolean fileExists;
        //check if file name is provided
        if(args.length == 0){
            fileExists = false;
        }
        else{
            fileExists = true;
            fileName = args[0];
            fileName = fileName + ".txt";
            readFromFile(fileName);
        }

        new MainFrame(fileName);
        //if the file does not exist
        if(!fileExists){
            MainFrame.printMessage("No file name provided. Portfolio will not be saved.");
        }
        //if the file exists
        else{
            MainFrame.printMessage("Portfolio will be saved to " + fileName);
        }
    }


    //buy method 
    public static void buyInvestment(String type, String symbol, String name, String quantity, String price){
        int quantityInt;
        double priceDouble;

    //convert quantity and price to proper data types
    try {
        quantityInt = Integer.parseInt(quantity);
        priceDouble = Double.parseDouble(price);
    } catch (NumberFormatException e) {
        MainFrame.printMessage("Invalid input: Quantity and price must be numeric values.");
        return;
    }
        //check if investment already exists
        for(int i = 0; i < investmentList.size(); i++){
            if(investmentList.get(i).getSymbol().equals(symbol)){
                //buy the investment
                try{
                    investmentList.get(i).buy(quantityInt, priceDouble);
                    MainFrame.printMessage("You have successfully bought " + quantity + " additional shares of " + investmentList.get(i).getName() + " at $" + price + " per share.");
                    return;
                    //catch invalid input
                }catch(IllegalArgumentException e){
                    MainFrame.printMessage("Invalid input");
                    return;
                }
            }
        }
        //if investment does not exist, create a new investment
        if(type.equals("Stock")){
            
            investmentList.add(new Stock(symbol, name, quantityInt, priceDouble));
            MainFrame.printMessage("You have successfully bought " + quantity + " shares of " + name + " at $" + price + " per share.");
            return;
        }
        else if(type.equals("Mutual Fund")){
            investmentList.add(new MutualFund(symbol, name, quantityInt, priceDouble));
            MainFrame.printMessage("You have successfully bought " + quantity + " shares of " + name + " at $" + price + " per share.");
            return;
        }
        
    }


    //sell method
    public static void sellInvestment(String symbol, String quantity, String price){
        int quantityInt;
        double priceDouble;
        //convert quantity and price to proper data types
        try {
            quantityInt = Integer.parseInt(quantity.trim());
            priceDouble = Double.parseDouble(price.trim());
        } catch (NumberFormatException e) {
            MainFrame.printMessage("Invalid input: Quantity and price must be numeric values.");
            return;
        }
        //check if investment exists in list
        for(int i = 0; i < investmentList.size(); i++){
            if(investmentList.get(i).getSymbol().equals(symbol)){
                //sell the investment
                try{
                    investmentList.get(i).sell(quantityInt, priceDouble);
                    MainFrame.printMessage("You have sold " + quantity + " shares of " + investmentList.get(i).getName());  
                    if(investmentList.get(i).getQuantity() == 0){
                        deleteInvestment(i);
                        return;
                    }
                    return;
                    //catch invalid input
                }catch(IllegalArgumentException e){
                    MainFrame.printMessage("Invalid input");
                    return;
                }
            }
        }
        MainFrame.printMessage("Investment not found");
    }

    //get gain method
    public static void getGain(){
        double totalGain = 0;
        //calculate total gain
        for(int i = 0; i < investmentList.size(); i++){
            totalGain += investmentList.get(i).getGain();
            //print gain for each investment
            MainFrame.printMessage("Gain for " + investmentList.get(i).getName() + ": $" + investmentList.get(i).getGain());
        }
        //print total gain
        GainPanel.setGainLabel(totalGain);
    }

    //update price method
    public static void updatePrice(String symbol, String price){
        double priceDouble;
        //convert price to proper data type
        try{
            priceDouble = Double.parseDouble(price);
        //catch invalid input
        }catch(NumberFormatException e){
            MainFrame.printMessage("Invalid input: Price must be a numeric value.");
            return;
        }
        //update price for investment
        for(int i = 0; i < investmentList.size(); i++){
            if(investmentList.get(i).getSymbol().equals(symbol)){
                investmentList.get(i).updatePrice(priceDouble);
                MainFrame.printMessage("Price for " + investmentList.get(i).getName() + " has been updated to $" + price);
                return;
            }
        }
        
    }
    //search method 
    public static void search(String symbol, String keywords, String lowPrice, String highPrice){
        double highPriceDouble;
        double lowPriceDouble;
        boolean found = false;
        symbol = symbol.toLowerCase().trim();
        keywords = keywords.toLowerCase().trim();
        //if all fields are empty
        if(symbol.isEmpty() && keywords.isEmpty() && lowPrice.isEmpty() && highPrice.isEmpty()){
            for(int i = 0; i < investmentList.size(); i++){
                MainFrame.printMessage(investmentList.get(i).toString());
                found = true;
            }
            return;
        }

        //if only low price is empty
        if(lowPrice.isEmpty()){
            lowPriceDouble = 0;
        }
        else{
            try{
                lowPriceDouble = Double.parseDouble(lowPrice);
            }catch(NumberFormatException e){
                MainFrame.printMessage("Invalid input: Low price must be a numeric value.");
                return;
            }
        }

        //if only high price is empty
        if(highPrice.isEmpty()){
            highPriceDouble = Double.MAX_VALUE;
        }
        else{
            try{
                highPriceDouble = Double.parseDouble(highPrice);
            }catch(NumberFormatException e){
                MainFrame.printMessage("Invalid input: High price must be a numeric value.");
                return;
            }
        }
        //if only keywords are empty
        if(keywords.isEmpty()){
            for(int i = 0; i < investmentList.size(); i++){
                double price = investmentList.get(i).getPrice();
                if((symbol.isEmpty() || investmentList.get(i).getSymbol().toLowerCase().equals(symbol))
                && (lowPrice.isEmpty() && highPrice.isEmpty() || (price>= lowPriceDouble && price <= highPriceDouble))){
                    MainFrame.printMessage(investmentList.get(i).toString());
                    found = true;
                }
            }
        }
        //else search by keywords
        else{
            Set<Integer> resultPositions = keywordSearch(keywords);
            for(int i = 0; i < investmentList.size(); i++){
                double price = investmentList.get(i).getPrice();
                if((symbol.isEmpty() || investmentList.get(i).getSymbol().toLowerCase().equals(symbol))
                && (lowPrice.isEmpty() && highPrice.isEmpty() || (price >= lowPriceDouble && price <= highPriceDouble))
                && resultPositions.contains(i)){
                    MainFrame.printMessage(investmentList.get(i).toString());
                    found = true;
                }
            }
        }
        //if no investments are found
        if(!found){
            MainFrame.printMessage("No investments found");
            return;
        }
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
    //get investment list method
    public static ArrayList<Investment> getInvestmentList(){
        return investmentList;
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
