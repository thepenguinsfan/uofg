 package eportfolio;

 public class Investment {
    protected String symbol;
    protected String name;
    protected int quantity;
    protected double price;
    //constructor
    public Investment(String symbol, String name, int quantity, double price) {
        this.symbol = symbol;
        this.name = name;
        this.quantity = quantity;
        this.price = price;
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
    //shared methods
    
    //update price method
    public void updatePrice(double newPrice) {
        this.price = newPrice;
    }

    //toString method
    public String toString() {
        return "Symbol: " + symbol + "\nName: " + name + "\nQuantity: " + quantity + "\nPrice: " + price;
    }

    //equals method
    public boolean equals(Investment other){
        return this.symbol.equals(other.symbol);
    }

 }