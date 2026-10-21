package discussionboard;
import java.util.List;
import java.util.ArrayList;
public class PollPost extends Post{
    private String options;
    private List <Integer> votes;

    //initial constructor
    public PollPost(String title, User author, int id, String options){
        super(title, author, id);
        this.options = options;
        this.votes = new ArrayList <Integer>();
        String [] optionsArray = options.split(";");
        for(int i = 0; i < optionsArray.length; i++){
            votes.add(0);
        }
    }
    //constructor for when votes are provided
    public PollPost(String title, User author, int id, String options, List<Integer> votes) {
        super(title, author, id);
        this.options = options;
        this.votes = votes; 
    }

    public String getOptions(){
        return options;
    }

    public List <Integer> getVotes(){
        return votes;
    }

    //add vote to the option
    public void addVote(int option){
        votes.set(option, votes.get(option) + 1);
    }
    //print the options and the votes
    public void printOptions() {
        String[] optionsArray = getOptions().split(";");
        for (int i = 0; i < optionsArray.length; i++) {
            String[] optionAndCount = optionsArray[i].split(":");
            String optionText = optionAndCount[0];
            int voteCount = votes.get(i);
            System.out.println("Option " + (i + 1) + ": " + optionText + " - Votes: " + voteCount);
        }
    }
    
}
