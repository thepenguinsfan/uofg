# CIS*4150 Assignment 4 Questions (18 marks)

Source: *Introduction to Software Testing*, 2nd Edition, Chapter 7.

The questions below are reworded from the textbook. See the PDF page listed with each one for the exact wording.

---

## Section 7.2.2, Question 6 (b) and (c) [4 marks]

PDF pages 178-179. Use the graph in Figure 7.2 (PDF page 161).

**Graph (edge list read from Figure 7.2)**

- N = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
- N0 = {1, 2, 3}
- Nf = {8, 9, 10}
- E = {(1, 4), (1, 5), (2, 5), (3, 6), (3, 7), (4, 8), (5, 8), (5, 9), (6, 2), (6, 10), (7, 10), (9, 6)}

**(b)** [2 marks] Give test paths on this graph that satisfy Node Coverage but do not satisfy Edge Coverage.

**(c)** [2 marks] Give test paths on this graph that satisfy Edge Coverage but do not satisfy Prime Path Coverage.

---

## Section 7.3, Question 1 (a), (b), and (c) [8 marks]

PDF pages 202-203. Use this program fragment, which the textbook labels with node numbers:

| Node | Statement | Context |
|------|-----------|---------|
| 1 | `w = x;` | Followed by `if (m > 0)` |
| 2 | `w++;` | Then-branch of `if (m > 0)` |
| 3 | `w = 2*w;` | Else-branch of `if (m > 0)` |
| 4 | No executable statement | Comes after the first if-else, followed by `if (y <= 10)` |
| 5 | `x = 5*y;` | Then-branch of `if (y <= 10)` |
| 6 | `x = 3*y+5;` | Else-branch of `if (y <= 10)` |
| 7 | `z = w + x;` | Comes after the second if-else |

**(a)** [4 marks] Draw the control flow graph for the fragment, using the node numbers above.

**(b)** [2 marks] Identify the nodes where variable *w* is defined.

**(c)** [2 marks] Identify the nodes where variable *w* is used.

---

## Section 7.3, Question 2 [6 marks]

PDF page 203.

Choose a commercial code coverage tool (a free trial is fine), then install it and run it on some software. The software can be an example from the textbook, something from a work environment, or something found online.

Write a short report on your experience with the tool, including any problems you had installing or using it. The main grading criterion is that you actually collected coverage data for a reasonable set of tests on a program.
