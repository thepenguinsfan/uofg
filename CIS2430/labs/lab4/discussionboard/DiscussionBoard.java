/*Name: Luc Levesque
  ID: 1238403
  Compilation: javac discussionboard/DiscussionBoard.java
  Run: java discussionboard/DiscussionBoard cis2430
*/

package discussionboard;
import java.util.Scanner;
import java.util.Set;
import java.util.ArrayList;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.HashMap;
import java.util.HashSet;
public class DiscussionBoard {
    private static ArrayList<User> userList = new ArrayList<User>();
    private static ArrayList<Post> postList = new ArrayList<Post>();
    private static HashMap<String, ArrayList<Integer>> nameIndex = new HashMap<>();

    public static void main(String[] args) {
        //get the file path from the command line argument
        String filePath = "";
        //check if file name is provided
        if(args.length == 0){
            System.out.println("No file name provided");
        }
        else{
            String fileName = args[0];
            filePath = "./boards/" + fileName + ".dboard";
            //load the discussion board from the file
            loadDiscussionBoard(filePath);
        }
        menu();
        Scanner input = new Scanner(System.in);
        String userChoice = input.nextLine();
        int choice = isInteger(userChoice);

        //loop until user ends the program
        while(choice != 5){
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

                default:
                    System.out.println("Invalid choice");
            }
            menu();
            userChoice = input.nextLine();
            choice = isInteger(userChoice);
        }
        saveDiscussionBoard(filePath);
        System.out.println("Goodbye");
        
        
    }

    //display menu
    public static void menu(){
        System.out.println(" (1) Create new user");
        System.out.println(" (2) Create new post");
        System.out.println(" (3) View all posts");
        System.out.println(" (4) View all posts with a given username");
        System.out.println(" (5) End program");
    }
    //create a new user
    public static void createUser(){
        System.out.println("Enter full name: ");
        Scanner input = new Scanner(System.in);
        String fullName = input.nextLine();
        fullName = isString(fullName);
        System.out.println("Enter username: ");
        String userName = input.nextLine().trim();
        //check if username already exists
        for(int i = 0; i < userList.size(); i++){
            if(userList.get(i).getUserName().equals(userName)){
                System.out.println("Username already exists");
                return;
            }
        }
        //if userName is empty, use first name as username
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
            User newUser = new User(fullName, firstName);
            addUser(newUser);
            System.out.println("Username is " + firstName);
            System.out.println("User created successfully");
            return;
        }
        //split username into separate words
        String[] userNameSplit = userName.split("[ ]+");
        String userNameFinal = userNameSplit[0];
        System.out.println("Username is " + userNameFinal);
        //create new user

        try{
            User newUser = new User(fullName, userNameFinal);
            addUser(newUser);
        }catch(IllegalArgumentException e){
            System.out.println("Invalid user");
            return;
        }
        System.out.println("User created successfully");

    }

    //create a new post
    public static void createPost(){
        System.out.println("Enter username: ");
        Scanner input = new Scanner(System.in);
        String userName = input.nextLine();
        userName = isString(userName);
        User author = null;
        //search for the user
        for(int i = 0; i < userList.size(); i++){
            if(userList.get(i).getUserName().equals(userName)){
                author = userList.get(i);
                break;
            }
        }
        //if user not found
        if(author == null){
            System.out.println("User not found. Please register to make a post.");
            return;
        }
        //create new ID for the post
        int postId = postList.size();
        //ensure post ID is unique
        for(int i = 0; i < postList.size(); i++){
            if(postList.get(i).getId() == postId){
                postId++;
            }
        }
        System.out.println("Enter title: ");
        String title = input.nextLine();
        System.out.println("Enter content: ");
        String content = input.nextLine();
        try{
            Post newPost = new TextPost(title, author, postId, content);
            postList.add(newPost);
            System.out.println("Post created successfully");
        }catch(IllegalArgumentException e){
            System.out.println("Invalid post");
            return;
        }
    }


    //view all posts
    public static void viewAllPosts(){
        if(postList.isEmpty()){
            System.out.println("No posts available");
            return;
        }
        for(int i = 0; i < postList.size(); i++){
            Post post = postList.get(i);
            System.out.println("Post ID: " + post.getId());
            System.out.println("Created by: " + post.getAuthor().getName() + "(@" + post.getAuthor().getUserName() + ")");
            System.out.println("Title: " + post.getTitle());
            //print content if a text post
            if(post instanceof TextPost){
                TextPost textPost = (TextPost) post;
                System.out.println("Content: " + textPost.getContent());
            }
            System.out.println('\n');
        }
    }

    //add username to hashmap
    public static void addUser(User user){
        userList.add(user);
        String keyWords[] = user.getUserName().toLowerCase().split(" ");
        for(int j = 0; j < keyWords.length; j++){
            nameIndex.computeIfAbsent(keyWords[j], k -> new ArrayList<>()).add(userList.size() - 1);
        }
        
    }

    //userName position search
    public static Set<Integer> searchUser(String userName){
        String keyWordList[] = userName.toLowerCase().split(" ");
        Set<Integer> resultPositions = new HashSet<>();

         //add the positions of the first keyWord to the result
        if (keyWordList.length > 0 && nameIndex.containsKey(keyWordList[0])) {
            resultPositions.addAll(nameIndex.get(keyWordList[0]));
        }

        //intersect the positions of the remaining keyWords with the result
        for (int i = 1; i < keyWordList.length; i++) {
            if (nameIndex.containsKey(keyWordList[i])) {
                resultPositions.retainAll(nameIndex.get(keyWordList[i]));
            } else {
                //if a keyWord is not found, clear the result
                resultPositions.clear();
                break;
            }
        }
        //return the indices of the posts that contain the username
        return resultPositions;
    }

    //view posts by user
    public static void viewPostsByUser(){
        System.out.println("Enter username to search for: ");
        Scanner input = new Scanner(System.in);
        String userName = input.nextLine();
        userName = isString(userName);
        Set<Integer> resultPositions = searchUser(userName);
        if(resultPositions.isEmpty()){
            System.out.println("No posts found for " + userName);
            return;
        }
        for(int i = 0; i < postList.size(); i++){
            if(resultPositions.contains(i)){
                Post post = postList.get(i);
                System.out.println("Post ID: " + post.getId());
                System.out.println("Created by: " + post.getAuthor().getName() + "(@" + post.getAuthor().getUserName() + ")");
                System.out.println("Title: " + post.getTitle());
                //print content if a text post
                if(post instanceof TextPost){
                    TextPost textPost = (TextPost) post;
                    System.out.println("Content: " + textPost.getContent());
                }
                System.out.println('\n');
            }
        }
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

    //save discussion board to file
    public static void saveDiscussionBoard(String filePath){
        File fileName = new File(filePath);
        try{
            FileWriter writer = new FileWriter(fileName);
            for(int i = 0; i < postList.size(); i++){
                Post post = postList.get(i);
                if(post instanceof TextPost){
                    TextPost textPost = (TextPost) post;
                    writer.write("TextPost\n");
                    writer.write(post.getId() + "\n");
                    writer.write(post.getTitle() + "\n");
                    writer.write(post.getAuthor().getName() + "\n");
                    writer.write(post.getAuthor().getUserName() + "\n");
                    writer.write(textPost.getContent() + "\n");
                }
            }
            writer.close();
        }catch (IOException  e){
            System.out.println("Error saving file");
            return;
        }
    }

    //load discussion board from file
    public static void loadDiscussionBoard(String filePath){
        File fileName = new File (filePath);
        try{
            FileReader fileReader = new FileReader(fileName);
            Scanner reader = new Scanner(fileReader);
            String line;
            //while there are more lines in the file
            while(reader.hasNextLine()){
                //trim the line read
                line = reader.nextLine().trim();
                //check if the line is a text post
                if(line.equals("TextPost")){
                    int id = Integer.parseInt(reader.nextLine());
                    String title = reader.nextLine();
                    String authorName = reader.nextLine();
                    String authorUserName = reader.nextLine();
                    String content = reader.nextLine();
                    User author = new User(authorName, authorUserName);
                    addUser(author);
                    Post newPost = new TextPost(title, author, id, content);
                    postList.add(newPost);
                }
            }
            reader.close();
        }catch(FileNotFoundException e){
            System.out.println("File not found");
            return;
        }
    }
}