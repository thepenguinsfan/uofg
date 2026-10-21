package eportfolio;

public class Stock {
    
    private String symbol;
    private String name;
    private int quantity;
    private double price;
    private double bookValue;

    private static double commission = 9.99;

    //constructor
    public Stock(String symbol, String name, int quantity, double price){
        this.symbol = symbol;
        this.name = name;
        this.quantity = quantity;
        this.price = price;
        this.bookValue = quantity * price + commission;
    }

    //getters
    public String getSymbol() {
        return symbol;
    }

    public String getName() {
        return name;
    }

    public int getQuantity() {
        return quantity;
    }   

    public double getPrice() {
        return price;
    }

    public double getBookValue() {
        return bookValue;
    }

    //buy method
    public void buy(int quantity, double price){
        // Calculate the new book value
        double newBookValue = (quantity * price) + commission;
        // Update the book value
        this.bookValue += newBookValue;
        // Update the quantity
        this.quantity += quantity;
        // Update the price
        this.price = price; 
    }

    //sell method
    public void sell(int quantity, double price){
        //calculate remaining quantity
        int remainingQuantity = this.quantity - quantity;
        //calculate remaining book value
        double bookValueRemaining = this.bookValue * ((double) remainingQuantity / this.quantity);
        //calculate book value sold
        double bookValueSold = this.bookValue - bookValueRemaining;
        //calculate payment
        double payment = (quantity * price) - commission;
        //calculate gain
        double gain = payment - bookValueSold;
        String gainString = String.format("%.2f", gain);
        //update quantity
        this.quantity = remainingQuantity;
        //update book value
        this.bookValue = bookValueRemaining;
        //update price 
        this.price = price;
        
        if(gain < 0){
            System.out.println("You have sold " + quantity + " shares of " + name + " for $" + payment + " with a loss of $" + gainString);
        }
        else{
            System.out.println("You have sold " + quantity + " shares of " + name + " for $" + payment + " with a gain of $" + gainString);
        }
    }
    
    //update price method
    public void updatePrice(double newPrice) {
        this.price = newPrice;
    }

    //calculate gain method
    public double getGain(){
        return (this.quantity * this.price) - commission - this.bookValue;
    }

    //toString method
    public String toString(){
        return "Symbol: " + symbol + "\nName: " + name + "\nQuantity: " + quantity + "\nPrice: " + price + "\nBook Value: " + bookValue;
    }

    //equals method
    public boolean equals(Stock other){
        return this.symbol.equals(other.symbol);
    }
}
