import java.util.*;

public class Main {
    public static void main(String[] args) {

    }

    /*
     * Chapter 3, Question 6
     * PrimeNumbers class from the textbook (contains a fault on purpose).
     */
    public static class PrimeNumbers implements Iterable<Integer> {
        private List<Integer> primes = new ArrayList<Integer>();

        public void computePrimes(int n) {
            int count = 1; // count of primes
            int number = 2; // number tested for primeness
            boolean isPrime; // is this number a prime
            while (count <= n) {
                isPrime = true;
                for (int divisor = 2; divisor <= number / 2; divisor++) {
                    if (number % divisor == 0) {
                        isPrime = false;
                        break; // for loop
                    }
                }
                if (isPrime && (number % 10 != 9)) { // FAULT
                    primes.add(number);
                    count++;
                }
                number++;
            }
        }

        @Override
        public Iterator<Integer> iterator() {
            return primes.iterator();
        }

        @Override
        public String toString() {
            return primes.toString();
        }
    }

    /*
     * Chapter 3, Question 8
     * Min class from the textbook (Figure 3.2).
     */
    public static class Min {
        /**
         * Returns the minimum element in a list
         * @param list Comparable list of elements to search
         * @return the minimum element in the list
         * @throws NullPointerException (NPE) if list is null or
         *         if any list elements are null
         * @throws ClassCastException (CCE) if list elements are not mutually comparable
         * @throws IllegalArgumentException if list is empty
         */
        public static <T extends Comparable<? super T>> T min(List<? extends T> list) {
            if (list.size() == 0) {
                throw new IllegalArgumentException("Min.min");
            }

            Iterator<? extends T> itr = list.iterator();
            T result = itr.next();

            if (result == null) throw new NullPointerException("Min.min");

            while (itr.hasNext()) {
                T comp = itr.next();
                if (comp.compareTo(result) < 0) { // throws NPE, CCE as needed
                    result = comp;
                }
            }
            return result;
        }
    }
}
