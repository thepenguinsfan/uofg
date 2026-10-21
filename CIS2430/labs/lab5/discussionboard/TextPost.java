package discussionboard;

public class TextPost extends Post {
    private String content;

    public TextPost(String title, User author, int id, String content){
        super(title, author, id);
        this.content = content;
    }
    //getters
    public String getContent(){
        return content;
    }



    
}
