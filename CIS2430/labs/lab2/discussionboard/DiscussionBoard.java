/*Name: Luc Levesque
  ID: 1238403
  Compilation: javac discussionboard/DiscussionBoard.java
  Run: java discussionboard/DiscussionBoard
  */

package discussionboard;
import java.util.Scanner;
import java.util.ArrayList;
public class DiscussionBoard{
    private static ArrayList<User> userList = new ArrayList<User>();
    private static ArrayList<Post> postList = new ArrayList<Post>();
    public static void main(String[] args){
        //display menu
        menu();
        Scanner input = new Scanner(System.in);
        int choice = input.nextInt();
        //loop until user ends the program
        while(choice != 6){
            switch(choice){
                case 1:
                    createUser();
                    break;
                case 2:
                    createPost();
                    break;
                case 3:
                    viewAllPosts();
                    break;  
                case 4:
                    viewPostsByUser();
                    break;
                case 5:
                    viewPostsByKeyword();
                    break;
                default:    
                    System.out.println("Invalid choice");
            }
            menu();
            choice = input.nextInt();
    }
    System.out.println("Goodbye");
}
    //method to display menu
    public static void menu(){
        System.out.println(" (1) Create new user");
        System.out.println(" (2) Create new post");
        System.out.println(" (3) View all posts");
        System.out.println(" (4)View all posts with a given username");
        System.out.println(" (5)View all posts with a given keyword");
        System.out.println(" (6) End program");
}
    //method 1: create a new user
    public static void createUser(){
        System.out.println("Enter full name: ");
        Scanner input = new Scanner(System.in);
        String fullName = input.nextLine();
        String fullNameTrim = fullName.trim();
        //check if full name is empty
        if(fullNameTrim.isEmpty()){
            System.out.println("Full name cannot be empty");
            return;
        }
        System.out.println("Enter username: ");
        //remove leading and trailing whitespaces
        String userName = input.nextLine().trim();
        //check if username already exists
        for(int i = 0; i < userList.size(); i++){
            if(userList.get(i).getUserName().equals(userName)){
                System.out.println("Username already exists");
                return;
            }
        }
        //if username is empty, use the first name as username
        if(userName.isEmpty()){
            String[] fullNameSplit = fullName.split(" ");
            String firstName = fullNameSplit[0].toLowerCase();
            //check if username already exists
            for(int i = 0; i < userList.size(); i++){
                if(userList.get(i).getUserName().equals(firstName)){
                    System.out.println("Username already exists");
                    return;
                }
            }
            //create new user
            User newUser = new User();
            newUser.setName(fullName);
            newUser.setUserName(firstName);
            userList.add(newUser);
            System.out.println("Username is " + firstName);
            System.out.println("User created successfully");
            return;
        }

        //split username into separate words
        String[] userNameSplit = userName.split("[ ]+");
        String userNameFinal = userNameSplit[0];
        System.out.println("Username is " + userNameFinal);
        //create new user
        User newUser = new User();
        newUser.setName(fullName);
        newUser.setUserName(userNameFinal);
        userList.add(newUser);
        System.out.println("User created successfully");
    }

    //method 2: create a new post
    public static void createPost(){
        System.out.println("Enter username: ");
        Scanner input = new Scanner(System.in);
        String username = input.nextLine();
        User author = null;
        //search for the user
        for(int i = 0; i < userList.size(); i++){
            if(userList.get(i).getUserName().equals(username)){
                author = userList.get(i);
                break;
            }
        }
        //if user not found
        if(author == null){
            System.out.println("User not found. Please register to make a post.");
            return;
        }
        //create new post
        System.out.println("Enter title: ");
        String title = input.nextLine();
        System.out.println("Enter content: ");
        String content = input.nextLine();
        Post newPost = new Post();
        newPost.setAuthor(author);
        newPost.setTitle(title);
        newPost.setContent(content);
        postList.add(newPost);
        System.out.println("Post created successfully");
    }

    //method 3: view all posts
    public static void viewAllPosts(){
        for(int i = 0; i < postList.size(); i++){
            System.out.println("Created by: " + postList.get(i).getAuthor().getName() + "(@" + postList.get(i).getAuthor().getUserName() + ")");
            System.out.println("Title: " + postList.get(i).getTitle());
            System.out.println("Content: " + postList.get(i).getContent());
        }
    }
    
    //method 4: view posts by a specific user
    public static void viewPostsByUser(){
        System.out.println("Enter the username to search for: ");
        Scanner input = new Scanner(System.in);
        String username = input.nextLine();
        boolean found = false;
        for(int i = 0; i < postList.size(); i++){
            if(postList.get(i).getAuthor().getUserName().equals(username)){
                System.out.println("Created by: " + postList.get(i).getAuthor().getName() + "(@" + postList.get(i).getAuthor().getUserName() + ")");
                System.out.println("Title: " + postList.get(i).getTitle());
                System.out.println("Content: " + postList.get(i).getContent());
                found = true;
            }
        }
        if(!found){
            System.out.println("No posts found");
        }
    }

    //method 5: view posts by a specific keyword
    public static void viewPostsByKeyword(){
        System.out.println("Enter the keyword to search for:");
        Scanner input = new Scanner(System.in);
        String keyword = input.nextLine();
        //convert keyword to lowercase
        String lowerKeyword = keyword.toLowerCase();
        for(int i = 0; i < postList.size(); i++){
            //convert content to lowercase
            String lowerContent = postList.get(i).getContent().toLowerCase();
            //split content into separate words
            String contentWords[] = lowerContent.split("[ ]+");
            //search for keyword in content
            for(int k = 0; k < contentWords.length; k++){
                if(contentWords[k].equals(lowerKeyword)){
                    System.out.println("Created by: " + postList.get(i).getAuthor().getName() + "(@" + postList.get(i).getAuthor().getUserName() + ")");
                    System.out.println("Title: " + postList.get(i).getTitle());
                    System.out.println("Content: " + postList.get(i).getContent());
                    break;
                }
            }
        }
    }
}