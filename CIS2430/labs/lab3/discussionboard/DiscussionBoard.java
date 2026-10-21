/*Name: Luc Levesque
  ID: 1238403
  Compilation: javac discussionboard/DiscussionBoard.java
  Run: java discussionboard/DiscussionBoard cis2430
*/


package discussionboard;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileWriter;
import java.io.FileReader;
public class DiscussionBoard {

    private static ArrayList<User> userList = new ArrayList<User>();
    private static ArrayList<Post> postList = new ArrayList<Post>();
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
        int choice = input.nextInt();

        //loop until user ends the program
        while(choice != 8){
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
                    voteInPoll();
                    break;
                case 5:
                    viewPostsByUser();
                    break;
                case 6:
                    viewPostsByKeyword();
                    break;
                case 7:
                    //check if file name is provided
                    if(args.length == 0){
                        System.out.println("No file name provided");
                        break;
                    }
                    saveDiscussionBoard(filePath);
                    break;
                default:
                    System.out.println("Invalid choice");
            }
            menu();
            choice = input.nextInt();
        }
        System.out.println("Goodbye");
    }

    //method to display the menu
    public static void menu(){
        System.out.println(" (1) Create new user");
        System.out.println(" (2) Create new post");
        System.out.println(" (3) View all posts");
        System.out.println(" (4) Vote in poll");
        System.out.println(" (5) View all posts with a given username");
        System.out.println(" (6) View all posts with a given keyword");
        System.out.println(" (7) Save discussion board to file");
        System.out.println(" (8) End program");
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
        int postId = postList.size();
        //ensure post ID is unique
        for(int i = 0; i < postList.size(); i++){
            if(postList.get(i).getId() == postId){
                postId++;
            }
        }
        //get the type of post
        System.out.println("Enter type of post (text/poll): ");
        String postType = input.nextLine().trim();
        postType = postType.toLowerCase();
        //create a text post
        if(postType.equals("text")){
            System.out.println("Enter title: ");
            String title = input.nextLine();
            System.out.println("Enter content: ");
            String content = input.nextLine();
            Post newPost = new TextPost(title, author, postId, content);
            postList.add(newPost);
            System.out.println("Post created successfully");
        }
        //create a poll post
        else if (postType.equals("poll")){
            System.out.println("Enter title: ");
            String title = input.nextLine();
            System.out.println("Enter number of options: ");
            int numOptions = input.nextInt();
            input.nextLine();
            //get the options
            StringBuilder optionsString = new StringBuilder();
            for(int i = 0; i < numOptions; i++){
                //get the option
                System.out.println("Enter option " + (i+1) + ": ");
                String option = input.nextLine().trim();
                //append the option to the options string
                optionsString.append(option);
                //if not the last option, add a semicolon
                if(i < numOptions - 1){
                    optionsString.append(";");
                }
            }
            //create the poll post
            String options = optionsString.toString();
            Post newPost = new PollPost(title, author, postId, options);
            postList.add(newPost);
        }
        else{
            System.out.println("Invalid post type");
        }

    }

    //method 3: view all posts
    public static void viewAllPosts(){
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
            //print options if a poll post
            else if(post instanceof PollPost){
                PollPost pollPost = (PollPost) post;
                pollPost.printOptions();
            }
            System.out.println('\n');
        }
    }

    //method 4: vote in poll
    public static void voteInPoll(){
        System.out.println("Enter post ID: ");
        Scanner input = new Scanner(System.in);
        int postId = input.nextInt();
        boolean found = false;
        //check if post ID is valid
        if(postId < 0 || postId >= postList.size()){
            System.out.println("Invalid post ID");
            return;
        }
        Post post = null;
        //search for the post
        for(int i = 0; i < postList.size(); i++){
            if(postList.get(i).getId() == postId){
                if(postList.get(i) instanceof PollPost){
                    post = postList.get(i);
                    found = true;
                    break;
                }
                else{
                    System.out.println("Post is not a poll");
                    return;
                }
                
            }
        }
        if(!found){
            System.out.println("Post not found");
            return;
        }
        PollPost pollPost = (PollPost) post;
        pollPost.printOptions();
        System.out.println("Enter option number to vote: ");
        int option = input.nextInt();
        //check if option number is valid
        String options[] = pollPost.getOptions().split(";");
        if(option < 1 || option > options.length){
            System.out.println("Invalid option number");
            return;
        }
        //add vote to the option
        pollPost.addVote(option - 1);

    }

    //method 5: view all posts by a given user
    public static void viewPostsByUser(){
        System.out.println("Enter the username to search for: ");
        Scanner input = new Scanner(System.in);
        String username = input.nextLine().trim();
        username = username.toLowerCase();
        boolean found = false;
        //search for the user posts
        for(int i = 0; i < postList.size(); i++){
            if(postList.get(i).getAuthor().getUserName().toLowerCase().equals(username)){
                Post post = postList.get(i);
                System.out.println("Post ID: " + post.getId());
                System.out.println("Created by: " + post.getAuthor().getName() + "(@" + post.getAuthor().getUserName() + ")");
                System.out.println("Title: " + post.getTitle());
                //if post is a text post, print the content
                if(post instanceof TextPost){
                    TextPost textPost = (TextPost) post;
                    System.out.println("Content: " + textPost.getContent());
                }
                //if post is a poll post, print the options
                else if(post instanceof PollPost){
                    PollPost pollPost = (PollPost) post;
                    pollPost.printOptions();
                }
                System.out.println('\n');
                found = true;
            }
        }
        if(!found){
            System.out.println("No posts found for user: " + username);
        }
    }

    //method 6: view all posts with a given keyword
    public static void viewPostsByKeyword(){
        System.out.println("Enter the keyword to search for: ");
        Scanner input = new Scanner(System.in);
        String keyword = input.nextLine().trim();
        keyword = keyword.toLowerCase();
        boolean found = false;
        //check text posts
        for(int  i = 0; i < postList.size(); i++){
            if(postList.get(i) instanceof TextPost){
                TextPost textPost = (TextPost) postList.get(i);
                //convert content to lowercase
                String content = textPost.getContent().toLowerCase();
                //spilt content into individual words
                String contentWords[] = content.split("[ ]+");
                for(int j = 0; j < contentWords.length; j++){
                    //check if keyword is found
                    if(contentWords[j].equals(keyword)){
                        Post post = postList.get(i);
                        System.out.println("Post ID: " + post.getId());
                        System.out.println("Created by: " + post.getAuthor().getName() + "(@" + post.getAuthor().getUserName() + ")");
                        System.out.println("Title: " + post.getTitle());
                        System.out.println("Content: " + textPost.getContent());
                        System.out.println('\n');
                        found = true;
                        break;
                    }
                }
            }
            //check poll posts
            else if(postList.get(i) instanceof PollPost){
                PollPost pollPost = (PollPost) postList.get(i);
                String options = pollPost.getOptions().toLowerCase();
                //split options into individual options
                String optionsArray[] = options.split(";");
                for(int j = 0; j < optionsArray.length; j++){
                    //split option and vote count
                    String optionAndCount[] = optionsArray[j].split(":");
                    String option = optionAndCount[0];
                    //check if keyword is found
                    if(option.equals(keyword)){
                        Post post = postList.get(i);
                        System.out.println("Post ID: " + post.getId());
                        System.out.println("Created by: " + post.getAuthor().getName() + "(@" + post.getAuthor().getUserName() + ")");
                        System.out.println("Title: " + post.getTitle());
                        pollPost.printOptions();
                        System.out.println('\n');
                        found = true;
                        break;
                    }
                }
                
            }
        }
        if(!found){
            System.out.println("No posts found with keyword: " + keyword);
        }
    }

    //method 7: save discussion board to file
    public static void saveDiscussionBoard(String filePath){
        File fileName = new File(filePath);
        try{
            FileWriter writer = new FileWriter(fileName);
            for(int i = 0; i < postList.size(); i++){
                //write the post to the file
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
                else if(post instanceof PollPost){
                    PollPost pollPost = (PollPost) post;
                    writer.write("PollPost\n");
                    writer.write(post.getId() + "\n");
                    writer.write(post.getTitle() + "\n");
                    writer.write(post.getAuthor().getName() + "\n");
                    writer.write(post.getAuthor().getUserName() + "\n");
                    // Write options and their votes in the format "option1:votes1;option2:votes2;..."
                    String[] optionsArray = pollPost.getOptions().split(";");
                    List<Integer> votes = pollPost.getVotes();
                    StringBuilder optionsWithVotes = new StringBuilder();
                    for (int j = 0; j < optionsArray.length; j++) {
                        //append the option with the vote count
                        optionsWithVotes.append(optionsArray[j]).append(":").append(votes.get(j));
                        //if not the last option, add a semicolon
                        if (j < optionsArray.length - 1) {
                            optionsWithVotes.append(";"); 
                        }
                    }   
                    // Write the options and votes to the file
                    writer.write(optionsWithVotes.toString() + "\n"); 
                    }
                }
            writer.close();
        } catch (IOException  e){
            System.out.println("Error saving file");
            return;
        }
    }

    //method 8: load discussion board from file
    public static void loadDiscussionBoard(String filePath){
       
        File fileName = new File(filePath);
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
                    User author = new User();
                    author.setName(authorName);
                    author.setUserName(authorUserName);
                    userList.add(author);
                    Post newPost = new TextPost(title, author, id, content);
                    postList.add(newPost);
                }
                //check if the line is a poll post
                else if(line.equals("PollPost")){
                    int id = Integer.parseInt(reader.nextLine());
                    String title = reader.nextLine();
                    String authorName = reader.nextLine();
                    String authorUserName = reader.nextLine();
                    User author = new User();
                    author.setName(authorName);
                    author.setUserName(authorUserName);
                    userList.add(author);
                    String optionsWithVotes = reader.nextLine();
                    //split the options with votes
                    String[] optionsArray = optionsWithVotes.split(";");
                    StringBuilder options = new StringBuilder();
                    List<Integer> votes = new ArrayList<Integer>();
                    //split the options and votes
                    for(int i = 0; i < optionsArray.length; i++){
                        String optionAndCount[] = optionsArray[i].split(":");
                        String option = optionAndCount[0];
                        int voteCount = Integer.parseInt(optionAndCount[1]);
                        //append the option to the options string
                        options.append(option);
                        //add the votes to the list
                        votes.add(voteCount);
                        //if not the last option, add a semicolon
                        if(i < optionsArray.length - 1){
                            options.append(";");
                        }
                    }
                    Post newPost = new PollPost(title, author, id, options.toString(), votes);
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