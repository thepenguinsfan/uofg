/*Name: Luc Levesque
  ID: 1238403
  Compilation: javac discussionboard/DiscussionBoard.java
  Run: java discussionboard/DiscussionBoard cis2430
*/
package discussionboard;
import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;
public class DiscussionBoard {

    private static ArrayList<User> userList = new ArrayList<>();
    private static ArrayList<Post> postList = new ArrayList<>();
    private static JTextArea messagesArea;

    public static void main(String[] args){
        //create frame
        JFrame userFrame = new JFrame("Discussion Board");
        userFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        userFrame.setSize(800, 600);
        userFrame.setLayout(new BorderLayout());

        //create messages panel
        JPanel messagesPanel = new JPanel();
        messagesPanel.setLayout(new BorderLayout());
        messagesArea = new JTextArea();
        messagesArea.setEditable(false);
        //add scroll pane
        JScrollPane scrollPane = new JScrollPane(messagesArea);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);

        messagesPanel.add(scrollPane, BorderLayout.CENTER);

        //create input panels
        JPanel leftPanels = new JPanel(new CardLayout());

        //register user panel
        JPanel registerPanel = new JPanel();
        registerPanel.setLayout(null);
        //create register label
        JLabel registerLabel = new JLabel("Register User");
        registerLabel.setBounds(100, 20, 200, 40);
        registerPanel.add(registerLabel);
        //create name label
        JLabel fullNameLabel = new JLabel("Full Name");
        fullNameLabel.setBounds(110, 100, 200, 20);
        registerPanel.add(fullNameLabel);
        //create name field
        JTextField fullNameField = new JTextField();
        fullNameField.setBounds(50, 120, 200, 20);
        registerPanel.add(fullNameField);
        //create username label
        JLabel usernameLabel = new JLabel("Username");
        usernameLabel.setBounds(110, 160, 200, 20);
        registerPanel.add(usernameLabel);
        //create username field
        JTextField usernameField = new JTextField();
        usernameField.setBounds(50, 180, 200, 20);
        registerPanel.add(usernameField);
        //create register button
        JButton registerButton = new JButton("Register");
        registerButton.setBounds(5, 400, 290, 70);
        registerPanel.add(registerButton);
        //add action listener
        registerButton.addActionListener(e -> registerUser(fullNameField.getText(), usernameField.getText()));

        //create post panel
        JPanel postPanel = new JPanel();
        postPanel.setLayout(null);
        //create post label
        JLabel postLabel = new JLabel("Create Post");
        postLabel.setBounds(105, 15, 200, 40);
        postPanel.add(postLabel);
        //create username label
        JLabel usernameLabel2 = new JLabel("Username");
        usernameLabel2.setBounds(110, 70, 200, 20);
        postPanel.add(usernameLabel2);
        //create username field
        JTextField usernameField2 = new JTextField();
        usernameField2.setBounds(50, 90, 200, 20);
        postPanel.add(usernameField2);
        //create title label
        JLabel titleLabel = new JLabel("Title");
        titleLabel.setBounds(130, 130, 200, 20);
        postPanel.add(titleLabel);
        //create title field
        JTextField titleField = new JTextField();
        titleField.setBounds(50, 150, 200, 20);
        postPanel.add(titleField);
        //create post body label
        JLabel postBodyLabel = new JLabel("Post Body");
        postBodyLabel.setBounds(110, 190, 200, 20);
        postPanel.add(postBodyLabel);
        //create post body area
        JTextArea postBodyArea = new JTextArea(5,25);
        postBodyArea.setBounds(50, 210, 200, 100);
        postBodyArea.setLineWrap(true);
        postPanel.add(postBodyArea);
        //create post button
        JButton postButton = new JButton("Create Post");
        postButton.setBounds(5, 400, 290, 70);
        postPanel.add(postButton);
        //add action listener
        postButton.addActionListener(e -> createPost(usernameField2.getText(), titleField.getText(), postBodyArea.getText()));


        //search posts panel
        JPanel searchPanel = new JPanel();
        searchPanel.setLayout(null);
        //add username label
        JLabel usernameLabel3 = new JLabel("Username");
        usernameLabel3.setBounds(110, 185, 200, 20);
        searchPanel.add(usernameLabel3);
        //add username field
        JTextField usernameField3 = new JTextField();
        usernameField3.setBounds(50, 205, 200, 20);
        searchPanel.add(usernameField3);
        //add search button
        JButton searchButton = new JButton("Search");
        searchButton.setBounds(5, 400, 290, 70);
        searchPanel.add(searchButton);
        searchButton.addActionListener(e -> searchPosts(usernameField3.getText()));
        //add all 3 panels to the left panel
        leftPanels.add(registerPanel, "Register User");
        leftPanels.add(postPanel, "Create Post");
        leftPanels.add(searchPanel, "Search Posts");
        //add left panel and messages panel to split pane
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanels, messagesPanel);
        splitPane.setDividerLocation(300);
        //add split pane to frame
        userFrame.add(splitPane, BorderLayout.CENTER);

        //create menu bar
        JMenu menu = new JMenu("Options");
        JMenuItem registerUserItem = new JMenuItem("Register User");
        JMenuItem createPostItem = new JMenuItem("Create Post");
        JMenuItem searchPostsItem = new JMenuItem("Search Posts");

        //add menu items to menu
        menu.add(registerUserItem);
        menu.add(createPostItem);
        menu.add(searchPostsItem);

        //create menu bar
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(menu);
        userFrame.setJMenuBar(menuBar);

        //action Listeners
        registerUserItem.addActionListener(e -> changePanel(leftPanels, "Register User"));
        createPostItem.addActionListener(e -> changePanel(leftPanels, "Create Post"));
        searchPostsItem.addActionListener(e -> changePanel(leftPanels, "Search Posts"));

        //show frame
        userFrame.setVisible(true);

        //load from file
        //get the file path from the command line argument
        String filePath = "";
        //check if file name is provided
        if(args.length == 0){
            printMessage("No file name provided");
        }
        else{
            String fileName = args[0];
            filePath = "./boards/" + fileName + ".dboard";
            //load the discussion board from the file
            loadDiscussionBoard(filePath);
        }

        //save to file on close
        String finalFilePath = filePath;
        userFrame.addWindowListener(new java.awt.event.WindowAdapter() {
            @Override
            public void windowClosing(java.awt.event.WindowEvent e) {
                saveDiscussionBoard(finalFilePath);
                System.exit(0);
            }
        });
        
    }
    //change panel method
    private static void changePanel(JPanel panel, String panelName){
        CardLayout cardLayout = (CardLayout) panel.getLayout();
        cardLayout.show(panel, panelName);
    }

    //register user method
    public static void registerUser(String fullName, String username){
        fullName = fullName.trim();
        username = username.trim();

        if(fullName.isEmpty()){
            printMessage("Full Name cannot be empty");
            return;
        }
        //create username if it is not provided
        if(username.isEmpty()){
            String[] fullNameSplit = fullName.split(" ");
            username = fullNameSplit[0].toLowerCase();
            for(int i = 0; i < userList.size(); i++){
                if(userList.get(i).getUserName().equals(username)){
                    printMessage("Username already exists");
                    return;
                }
            }
        }
        //if username is provided, check if it already exists
        String[] usernameSplit = username.split("[ ]+");
        String usernameFinal = usernameSplit[0];

        for(int i = 0; i < userList.size(); i++){
            if(userList.get(i).getUserName().equals(usernameFinal)){
                printMessage("Username already exists");
                return;
            }
        }
        //create new user
        User user = new User(fullName, usernameFinal);
        userList.add(user);
        printMessage("Username is " + usernameFinal);
        printMessage("User registered successfully");
        
    }
    //create post method
    public static void createPost(String username, String title, String postBody){
        username = username.trim();
        title = title.trim();
        postBody = postBody.trim();
        User author = null;

        if(username.isEmpty()){
            printMessage("Username cannot be empty");
            return;
        }
        if(title.isEmpty()){
            printMessage("Title cannot be empty");
            return;
        }
        if(postBody.isEmpty()){
            printMessage("Post Body cannot be empty");
            return;
        }

        //check if the user exists
        for(int i = 0; i < userList.size(); i++){
            if(userList.get(i).getUserName().equals(username)){
                author = userList.get(i);
                break;
            }
        }
        //if user not found
        if(author == null){
            printMessage("User not found. Please register to make a post.");
            return;
        }
        //create post id
        int postId = postList.size();
        //ensure post ID is unique
        for(int i = 0; i < postList.size(); i++){
            if(postList.get(i).getId() == postId){
                postId++;
            }
        }
        //create post
        Post post = new TextPost(title, author, postId, postBody);
        postList.add(post);
        printMessage("Post created successfully");
    }

    //search posts method
    public static void searchPosts(String username){
        username = username.trim();

        if(username.isEmpty()){
            printMessage("Username cannot be empty");
            return;
        }
        username = username.toLowerCase();
        boolean found = false;
        //search for the user posts
        for(int i = 0; i < postList.size(); i++){
            if(postList.get(i).getAuthor().getUserName().toLowerCase().equals(username)){
                Post post = postList.get(i);
                printMessage("Post ID: " + post.getId());
                printMessage("Created by: " + post.getAuthor().getName() + "(@" + post.getAuthor().getUserName() + ")");
                printMessage("Title: " + post.getTitle());
                //if post is a text post, print the content
                if(post instanceof TextPost){
                    TextPost textPost = (TextPost) post;
                    printMessage("Content: " + textPost.getContent());
                }
                found = true;
            }
        }
        if(!found){
            printMessage("No posts found for user: " + username);
        }
    }
    //print message to the message area
    //helper method
    public static void printMessage(String message){
        messagesArea.append(message + "\n");
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
            printMessage("Error saving file");
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
                    userList.add(author);
                    Post newPost = new TextPost(title, author, id, content);
                    postList.add(newPost);
                }
            }
            reader.close();
        }catch(FileNotFoundException e){
            printMessage("File not found");
            return;
        }
    }
}