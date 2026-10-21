package eportfolio;

public class Stock extends Investment{
    private double bookValue;
    private static double commission = 9.99;
    
    //constructor
    public Stock(String symbol, String name, int quantity, double price) {
        super(symbol, name, quantity, price);
        this.bookValue = (quantity * price) + commission;
    }

    //getters
    public double getBookValue() {
        return bookValue;
    }
    //buy method
    public void buy(int quantity, double price){
        if (quantity <= 0) {
            throw new IllegalArgumentException("Quantity must be greater than 0.");
        }
    
        if (price <= 0) {
            throw new IllegalArgumentException("Price must be greater than 0.");
        }
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
        if (quantity <= 0) {
            throw new IllegalArgumentException("Quantity must be greater than 0.");
        }
    
        if (price <= 0) {
            throw new IllegalArgumentException("Price must be greater than 0.");
        }
    
        if (quantity > this.quantity) {
            throw new IllegalArgumentException("Not enough shares to sell.");
        }
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
        
    }

    //calculate gain method
    public double getGain(){
        return (this.quantity * this.price) - commission - this.bookValue;
    }

    //toString method
    public String toSString(){
        return symbol + "\n" + name + "\n" + quantity + "\n" + price;
    }
}