# CIS4150 Assignment 3

Textbook: *Introduction to Software Testing, 2nd Edition* (Ammann & Offutt), Chapter 6 and Chapter 7 Exercises.

Work in groups of 2 or 3. Total: **32 marks**.

---

## Chapter 6.1, Question 3 [6 marks]

Answer the following questions for the method `search()` below:

```java
public static int search (List list, Object element)
// Effects: if list or element is null throw NullPointerException
//    else if element is in the list, return an index
//    of element in the list; else return -1
//    for example, search ([3,3,1], 3) = either 0 or 1
//        search ([1,7,5], 2) = -1
```

Base your answer on the following characteristic partitioning:

```
Characteristic: Location of element in list
    Block 1: element is first entry in list
    Block 2: element is last entry in list
    Block 3: element is in some position other than first or last
```

**(a)** "Location of element in list" fails the disjointness property. Give an example that illustrates this.
search ([3, 4, 3], 3). Falls into blocks 1 and 2 as element is both the first and last entry in the list.
**(b)** "Location of element in list" fails the completeness property. Give an example that illustrates this.
search([1, 2, 3], 4). The element is not on the list, so it doesn't fall into any of the blocks.
**(c)** Supply one or more new partitions that capture the intent of "Location of element in list" but do not suffer from completeness or disjointness problems.

C1: element is first entry in list
    Block 1: True
    Block 2: False
C2: element is last entry in list
    Block 1: True
    Block 2: False
C3: number of times element appears in list
    Block 1: Appears no times
    Block 2: Appears once
    Block 3: Appears more than once

---

## Chapter 6.2, Question 4 — (a), (b), (c), (d), (e) [2 marks each; 10 marks total]

Answer the following questions for the method `intersection()` below:

```java
public Set intersection (Set s1, Set s2)
// Effects:  If s1 or s2 is null throw NullPointerException
//    else return a (non null) Set equal to the intersection
//    of Sets s1 and s2
```

```
Characteristic: Validity of s1
  - s1 = null
  - s1 = {}
  - s1 has at least one element

Characteristic: Relation between s1 and s2
  - s1 and s2 represent the same set
  - s1 is a subset of s2
  - s2 is a subset of s1
  - s1 and s2 do not have any elements in common
```

**(a)** Does the partition "Validity of s1" satisfy the completeness property? If not, give a value for s1 that does not fit in any block.

Yes as every possible value of s1 is either null, {}, or has at least one element. So s1 will never fall outside of these 3 blocks.


**(b)** Does the partition "Validity of s1" satisfy the disjointness property? If not, give a value for s1 that fits in more than one block.

Yes, the 3 blocks can't overlap as null isn't set at all, {} is set but has no elements, and the third block requires at least one element.


**(c)** Does the partition "Relation between s1 and s2" satisfy the completeness property? If not, give a pair of values for s1 and s2 that does not fit in any block.

No. For example: s1 = {1,2} and s2 = {2,3}. These sets do not represent the same set, neither are a subset of eachother and they have an element in common. This example does not fit in any of the blocks.

**(d)** Does the partition "Relation between s1 and s2" satisfy the disjointness property? If not, give a pair of values for s1 and s2 that fits in more than one block.

No. For example: s1 = {1,2} and s2 = {1,2}. These sets satisfy 3 blocks at the same time. They represent the same set, and they are subsets of one another.

**(e)** If the "Base Choice" criterion were applied to the two partitions (exactly as written), how many test requirements would result?

C1 has 3 blocks, so 3 - 1 = 2 tests
C2 has 4 blocks, so 4 - 1 = 3 tests

The total test requirements would be 1 + 2 + 3 = 6.

---

## Chapter 7.2.2, Question 5 [16 marks]

Answer questions a–g for the graph defined by the following sets:

```
N  = {1, 2, 3, 4, 5, 6, 7}
N0 = {1}
Nf = {7}
E  = {(1, 2), (1, 7), (2, 3), (2, 4), (3, 2), (4, 5), (4, 6), (5, 6), (6, 1)}
```

Also consider the following (candidate) test paths:

```
p1 = [1, 2, 4, 5, 6, 1, 7]
p2 = [1, 2, 3, 2, 4, 6, 1, 7]
p3 = [1, 2, 3, 2, 4, 5, 6, 1, 7]
```

**(a)** Draw the graph.

**(b)** List the test requirements for Edge-Pair Coverage. (Hint: You should get 12 requirements of length 2.)

[6,1,2], [6,1,7], [1,2,3], [1,2,4], [3,2,3], [3,2,4], [2,3,2], [2,4,5], [2,4,6], [4,5,6], [4,6,1], [5,6,1]

**(c)** Does the given set of test paths satisfy Edge-Pair Coverage? If not, state what is missing.

No. 
p1 covers [1,2,4], [2,4,5], [4,5,6], [5,6,1], [6,1,7]
p2 covers [1,2,3], [2,3,2], [3,2,4], [2,4,6], [4,6,1], [6,1,7]
p3 covers [1,2,3], [2,3,2], [3,2,4], [2,4,5], [4,5,6], [5,6,1], [6,1,7]

There is two missing requirements, [3,2,3] and [6,1,2].

**(d)** Consider the simple path `[3, 2, 4, 5, 6]` and test path `[1, 2, 3, 2, 4, 6, 1, 2, 4, 5, 6, 1, 7]`. Does the test path tour the simple path directly? With a sidetrip? If so, write down the sidetrip.

No the test path does not tour the simple path directly as after 3, 2, 4, the path goes to 6 and not 5. The test path tours with a sidetirp though, with the sidetrip being [4,6,1,2,4].

**(e)** List the test requirements for Node Coverage, Edge Coverage, and Prime Path Coverage on the graph.

Node Coverage: (1,2,3,4,5,6,7)

Edge Coverage: {(1,2), (1,7), (2,3), (2,4), (3,2), (4,5), (4,6), (5,6), (6,1)}

Prime Path Coverage

**(f)** List test paths from the given set that achieve Node Coverage but not Edge Coverage on the graph.

p3 visits all 7 nodes. However, it doesn't use the edge (4,6) so edge coverage is not achieved.

**(g)** List test paths from the given set that achieve Edge Coverage but not Prime Path Coverage on the graph.

{p1,p2} achieve edge coverage as p2 adds the edges that p1 misses ((2,3), (3,2) and (4,6)). These paths do not achieve prime path coverage as {list paths that arent toured}. {p2,p3} and {p1,p2,p3} also acheive edge coverage but not prime path coverage.
