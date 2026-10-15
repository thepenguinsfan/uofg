# CIS4150 Assignment 2

Textbook: *Introduction to Software Testing, 2nd Edition* (Ammann & Offutt), Chapter 3 Exercises (pp. 88–91).

Work in groups of 2 or 3. Total: **20 marks**.

The code referenced by both questions (`PrimeNumbers` and `Min`) is in [Main.java](Main.java).

---

## Chapter 3, Question 6 — (a), (b), (c), (d), (e) [2 marks each; 10 marks total]

Consider the following example class. `PrimeNumbers` has three methods. The first, `computePrimes()`, takes one integer input and computes that many prime numbers. `iterator()` returns an `Iterator` that will iterate through the primes, and `toString()` returns a string representation.

```java
public class PrimeNumbers implements Iterable<Integer>
{
   private List<Integer> primes = new ArrayList<Integer>();

   public void computePrimes (int n)
   {
      int count = 1; // count of primes
      int number = 2; // number tested for primeness
      boolean isPrime; // is this number a prime
      while (count <= n)
      {
         isPrime = true;
         for (int divisor = 2; divisor <= number / 2; divisor++)
         {
            if (number % divisor == 0)
            {
               isPrime = false;
               break; // for loop
            }
         }
         if (isPrime && (number % 10 != 9)) // FAULT
         {
            primes.add (number);
            count++;
         }
         number++;
      }
   }

   @Override public Iterator<Integer> iterator()
   {
      return primes.iterator();
   }

   @Override public String toString()
   {
      return primes.toString();
   }
}
```

`computePrimes()` has a fault that causes it **not** to include prime numbers whose last digit is 9 (for example, it omits 19, 29, 59, 79, 89, 109, ...). If possible, describe five tests. You can describe the tests as sequences of calls to the above methods, or briefly describe them in words. Note that the last two tests require the test oracle to be described.

**(a)** A test that does not reach the fault
computePrimes(0), toString().
The while (count <= n) loop sarts with count = 1.
Since 1 <= 0 is false, the loop never executes so the fault is never reached.
**(b)** A test that reaches the fault, but does not infect
computePrimes(3), then call toString
The loop runs so the fault is reached. However, only the numbers [2,3,5] are tested
so it gives the same result as the correct isPrime. Therefore, the state is never incorrect so there is no infection.
**(c)** A test that infects the state, but does not propagate 
No test can be written
**(d)** A test that propagates, but does not reveal
computePrimes(8)
**(e)** A test that reveals the fault
computePrimes(8)
If a test cannot be created, explain why.

---

## Chapter 3, Question 8 [10 marks]

Develop a set of data-driven JUnit tests for the `Min` program. These tests should be for normal, not exceptional, returns. Make your `@Parameters` method produce both `String` and `Integer` values.

**Assignment additions:**

- Create **at least 10 test cases** that include examples of **passes, errors, and failures**.
- Provide comments in your code explaining the purpose and expected outcome of each test. Comments should clearly describe whether the test is expected to **pass**, produce an **error**, or **fail**, and **why**.
- Capture and paste **screenshots** of your code and the test results. Screenshots should clearly show the code, the executed tests, and their respective outcomes. The goal is to demonstrate thorough testing and an understanding of the results for each test case.

The `Min` program (Figure 3.2):

```java
import java.util.*;
public class Min
{
   /**
    * Returns the minimum element in a list
    * @param list Comparable list of elements to search
    * @return the minimum element in the list
    * @throws NullPointerException (NPE) if list is null or
    *         if any list elements are null
    * @throws ClassCastException (CCE) if list elements are not mutually comparable
    * @throws IllegalArgumentException if list is empty
    */
   public static <T extends Comparable<? super T>> T min (List<? extends T> list)
   {
      if (list.size() == 0)
      {
         throw new IllegalArgumentException ("Min.min");
      }

      Iterator<? extends T> itr = list.iterator();
      T result = itr.next();

      if (result == null) throw new NullPointerException ("Min.min");

      while (itr.hasNext())
      {
         T comp = itr.next();
         if (comp.compareTo (result) < 0)
         {  // throws NPE, CCE as needed
            result = comp;
         }
      }
      return result;
   }
}
```

---

## Submission Checklist

- [ ] Fill answers into `Assignment_2.docx`
- [ ] Complete and sign group member details and LLM declaration
- [ ] Save as `lastname_firstname_lastname_firstname_lastname_firstname.pdf`
- [ ] Only the first listed member submits to the CourseLink Dropbox
