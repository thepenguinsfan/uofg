package discussionboard;  

public class User{
    private String name;
    private String userName;

    //constructor
    public User(String name, String userName) {
        this.name = name;
        this.userName = userName;
    }


    //getters
    public String getName(){
        return name;
    }

    public String getUserName(){
        return userName;
    }

  
}