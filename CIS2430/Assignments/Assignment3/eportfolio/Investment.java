package eportfolio;

 public abstract class Investment {
    protected String symbol;
    protected String name;
    protected int quantity;
    protected double price;

    //constructor
    public Investment(String symbol, String name, int quantity, double price) {
        if (symbol == null || symbol.isEmpty() ||name == null || name.isEmpty() || quantity < 0 || price < 0){
            throw new IllegalArgumentException("Invalid input");
        }

        this.symbol = symbol;
        this.name = name;
        this.quantity = quantity;
        this.price = price;
    }
    //abstract methods
    public abstract void buy(int quantity, double price);
    public abstract void sell(int quantity, double price);
    public abstract double getGain();

    
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
        if(newPrice <= 0){
            throw new IllegalArgumentException("Price must be greater than 0.");
        }
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
