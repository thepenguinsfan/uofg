package discussionboard;

public class Post{
    protected String title;
    protected User author;
    protected int id;

    public Post(String title, User author, int id){
        this.title = title;
        this.author = author;
        this.id = id;
    }

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