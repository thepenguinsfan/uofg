package discussionboard;

public class TextPost extends Post {
    private String content;

    public TextPost(String title, User author, int id, String content){
        super(title, author, id);
        if(content == null || content.isEmpty()){
            throw new IllegalArgumentException("Content cannot be null or empty");
        }
    }
    //getters
    public String getContent(){
        return content;
    }



    
}