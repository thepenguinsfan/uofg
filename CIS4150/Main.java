import java.util.Vector;

public class Main {
    public static void main(String[] args) {
        Vector a = new Vector();
        Vector b = new Vector();

        a.add(1);
        a.add(2);
        a.add(3);

        b.add(1);
        b.add(4);
        b.add(5);
        b.add(3);

        System.out.println(union(a, b)); 
    }

    public static Vector union(Vector a, Vector b){
        Vector result = new Vector();

        for(int i = 0; i < a.size(); i++){
            if(!result.contains(a.get(i))){
                result.add(a.get(i));
            }
        }

        for(int i = 0; i < b.size(); i++){
            if(!result.contains(b.get(i))){
                result.add(b.get(i));
            }
        }

        return result;
    }
}
