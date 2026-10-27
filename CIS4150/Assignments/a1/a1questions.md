**(a)** Write a Java method with the signature:

```java
public static Vector union(Vector a, Vector b)
```

The method should return a Vector of objects that are in either of the two argument Vectors.

**(b)** Upon reflection, you may discover a variety of defects and ambiguities in the given assignment. In other words, ample opportunities for faults exist. Describe as many possible faults as you can.

- Null arguments
    No specficiation on how to handle null vectors
- Duplicates
    "union" implicates a set but a vector is a list. There is no details on whether the method returns a set or list
- Equality
    We don't know what is considered equal. Example, Integer 1 and Double 1.0 are different because contains() uses equals(). 
- Ordering
    Specification gives no guidance on the ordering of the elements
- Type 
    A vector accepts mixed types of objects. We don't know if it should or not.
- Return Value
    We don't know if a new vector is returned, or what is returned if a or b is null or empty
> **Note:** Vector is a Java Collection class. If you are using another language, interpret Vector as a list.

**(c)** Create a set of test cases that you think would have a reasonable chance of revealing the faults you identified above. Document a rationale for each test in your test set. If possible, characterize all of your rationales in some concise summary. Run your tests against your implementation.

**(d)** Rewrite the method signature to be precise enough to clarify the defects and ambiguities identified earlier. You might wish to illustrate
