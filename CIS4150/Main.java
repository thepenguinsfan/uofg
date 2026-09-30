import java.util.Vector;

public class Main {
    public static void main(String[] args) {
        Vector a = new Vector();
        Vector b = new Vector();

        a.add("a");
        a.add(new String ("a"));

        System.out.println(union(a, b)); 
    }




    /*
    * @param a first vector, cannot be null, can be empty
    * @param b second vector, cannot be null, can be empty
    * @return a new vector that is never null and never a or b itself. 
    *         The vector contains all elements of a and b, without duplicates.
    * @throws NullPointerException if a or b is null
    * 
    * Duplicates: The results contains no duplicates, including duplicates within a or b. 
    * Equality: Two elements x and y are considered equal iff x.equals(y) is true.
    * Ordering: The ordering in the result is elemets of a, in order of their appearance in a
    *           followed by elements of b that are not already in the result, in order of their appearance in b.
    * Type: Both a and b hold elements of type E, and the result holds elements of type E.
    * Return value: vectors a and b are not modified. If both are empty, the result is also empty.
    * 
    * Examples:
    *   union([1,2], [2,3])          returns [1,2,3]
    *   union([1,1,2], [3])          returns [1,2,3]
    *   union([1], [1.0])            returns [1,1.0]
    *   union([3,1,2], [5,4])        returns [3,1,2,5,4]
    *   union([1,"a"], [2.1,"b"])    returns [1,"a",2.1,"b"]   (E = Object)
    *   union([], [])                returns []
    *   union(null, [1,2])           throws NullPointerException
    */
    public static <E> Vector<E> union(Vector<? extends E> a, Vector<? extends E> b){
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
