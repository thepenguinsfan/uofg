package discussionboard;

public class Post{
    protected String title;
    protected User author;
    protected int id;

    public Post(String title, User author, int id){
        if(title == null || title.isEmpty()){
            throw new IllegalArgumentException("Title cannot be null or empty");
        }
        if(author == null){
            throw new IllegalArgumentException("Author cannot be null");
        }
        this.title = title;
        this.author = author;
        this.id = id;
    }

    //getters
    public String getTitle(){
        return title;
    }
   
    public User getAuthor(){
        return author;
    }

    public int getId(){
        return id;
    }


}