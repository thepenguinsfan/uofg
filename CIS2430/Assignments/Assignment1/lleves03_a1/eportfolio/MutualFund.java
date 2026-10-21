package eportfolio;

public class MutualFund {
    
    private String symbol;
    private String name;
    private int quantity;
    private double price;
    private double bookValue;

    private static double redemption = 45.00;

    //constructor
    public MutualFund(String symbol, String name, int quantity, double price){
        this.symbol = symbol;
        this.name = name;
        this.quantity = quantity;
        this.price = price;
        this.bookValue = quantity * price;
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
        double newBookValue = (quantity * price);
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
        double payment = (quantity * price) - redemption;
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
        return (this.quantity * this.price) - redemption - this.bookValue;
    }

    //toString method
    public String toString(){
        return "Symbol: " + symbol + "\nName: " + name + "\nQuantity: " + quantity + "\nPrice: " + price + "\nBook Value: " + bookValue;
    }

    //equals method
    public boolean equals(MutualFund other){
        return this.symbol.equals(other.symbol);
    }
}
