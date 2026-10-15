# CIS4150 Assignment 2 – Question 6 Draft Answers

Sources: Ch02-mdtd.pptx (slides 8–9, RIPR model) and Ch01-whyTest.pptx (slides 9–11, fault/error/failure).

**Background.** A **fault** is a static defect in the code. An **error** is an incorrect internal state. A **failure** is incorrect external behaviour. Under the RIPR model, a test can only observe a failure if it does all four of these:

1. **Reaches** the faulty location
2. **Infects** the program state, making it incorrect
3. **Propagates** the infection to the final state or output
4. **Reveals** the failure, meaning the test oracle actually checks the incorrect part

The fault is on line `if (isPrime && (number % 10 != 9))`. The correct code is `if (isPrime)`.

The primes in order are 2, 3, 5, 7, 11, 13, 17, **19**, 23, … so 19 is the 8th prime and the first one that ends in 9.

---

## (a) A test that does not reach the fault

Test: `computePrimes(0)`, then `toString()`. Expected: `"[]"`.

The `while (count <= n)` loop starts with `count = 1`. When n = 0 the condition `1 <= 0` is false, so the loop body never runs and the faulty `if` is never executed. Any n ≤ 0 (for example, a negative n) works the same way.

---

## (b) A test that reaches the fault but does not infect

Test: `computePrimes(3)`, then `toString()`. Expected: `"[2, 3, 5]"`.

The loop runs, so the faulty condition is evaluated and the fault is reached. The numbers tested are only 2, 3, 4 and 5. None of them is a prime ending in 9, so `isPrime && (number % 10 != 9)` gives the same result as the correct `isPrime` every time. The state is never incorrect, so there's no infection. Any n from 1 to 7 works, because the last number tested is at most 17.

n = 7 also tests 9. Since 9 isn't prime, `isPrime` is already false and both versions agree.

---

## (c) A test that infects the state but does not propagate

**This test can't be written.**

Infection happens only when `number` is a prime ending in 9 while `count <= n`, which first happens at number = 19 with n ≥ 8. At that point the correct program would add 19 to `primes`. The faulty program skips it and then goes on to add a later prime, such as 23, in its place.

- `primes` only ever has elements added and never removed or reordered. Once 19 is skipped it can never end up in the list.
- The loop always keeps going until `count > n`. So the list always ends with n elements, and one of them is the wrong value (23 or later instead of 19).

The infected state therefore always carries through to the final state of `primes`. No input infects without propagating.

---

## (d) A test that propagates but does not reveal

Test: `computePrimes(8)`.

- Expected final state: `[2, 3, 5, 7, 11, 13, 17, 19]`
- Actual final state: `[2, 3, 5, 7, 11, 13, 17, 23]`

The final state is incorrect, so the infection has propagated.

**Weak oracle:** use `iterator()` to check that the list has exactly 8 elements and that each element is prime. The actual list has 8 elements and every one is prime, so the test **passes**. The failure is in the final state, but the oracle never looks at the specific values, so it isn't revealed.

Other weak oracles that also pass:

- Checking only that the first element is 2
- Checking only that the list is in ascending order

---

## (e) A test that reveals the fault

Test: `computePrimes(8)`, then `assertEquals("[2, 3, 5, 7, 11, 13, 17, 19]", primes.toString())`.

**Oracle:** compare the whole list against the first 8 true primes. The actual output is `"[2, 3, 5, 7, 11, 13, 17, 23]"`, so the assertion **fails**. The oracle checks the exact part of the state that's wrong, so the failure is revealed.

An equivalent oracle is checking that the list contains 19.
