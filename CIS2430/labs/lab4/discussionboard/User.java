package discussionboard;  

public class User{
    private String name;
    private String userName;

    //constructor
    public User(String name, String userName) {
        if (name == null || name.isEmpty()) {
            throw new IllegalArgumentException("Name cannot be null or empty");
        }
        if (userName == null || userName.isEmpty()) {
            throw new IllegalArgumentException("Username cannot be null or empty");
        }
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