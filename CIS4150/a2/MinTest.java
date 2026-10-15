import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

public class MinTest {

    //Each row is { input list, expected minimum }.
    //The rows use both String and Integer values.
    static Stream<Arguments> parameters() {
        return Stream.of(
            //when the list is ["cat"], the minimum is expected to be "cat".
            //PASS: a single element is always the minimum.
            Arguments.of(Arrays.asList("cat"), "cat"),

            //when the list is ["dog", "cat"], the minimum is expected to be "cat".
            //PASS: the minimum is last, so the loop replaces result with it.
            Arguments.of(Arrays.asList("dog", "cat"), "cat"),

            //when the list is [7], the minimum is expected to be 7.
            //PASS: a single Integer is always the minimum.
            Arguments.of(Arrays.asList(7), 7),

            //when the list is [5, -3, 10], the minimum is expected to be -3.
            //PASS: negative numbers are compared correctly.
            Arguments.of(Arrays.asList(5, -3, 10), -3),

            //when the list is ["ant", "Bee"], the minimum is expected to be "ant".
            //FAILURE: compareTo is case-sensitive and uppercase sorts first ('B' = 66 < 'a' = 97), therefore Min.min returns "Bee".
            Arguments.of(Arrays.asList("ant", "Bee"), "ant"),

            // when the list is ["10", "9"], the minimum is expected to be "9".
            // FAILURE: Strings compare character by character, therefore "10" < "9" ('1' < '9'), so Min.min returns "10".
            Arguments.of(Arrays.asList("10", "9"), "9"),

            // when the list is [4, 2, 9], the minimum is expected to be 4.
            // FAILURE: the expected value is wrong on purpose, therefore Min.min returns 2.
            Arguments.of(Arrays.asList(4, 2, 9), 4),

            //when the list is ["cat", null], the minimum is expected to be "cat".
            //ERROR: calling compareTo on the null element throws a NullPointerException
            Arguments.of(Arrays.asList("cat", null), "cat"),

            // when the list is ["cat", 1], the minimum is expected to be "cat".
            // ERROR: a String and an Integer are not comparable, therefore Min.min throws a ClassCastException.
            Arguments.of(Arrays.asList("cat", 1), "cat"),

            // when the list is [], the minimum is expected to be 0
            // ERROR: Min.min throws an IllegalArgumentException for an empty list 
            Arguments.of(new ArrayList<Integer>(), 0)
        );
    }

    @ParameterizedTest(name = "{index}: min({0}) expected {1}")
    @MethodSource("parameters")
    void minReturnsSmallestElement(List list, Object expected) {
        assertEquals(expected, Min.min(list));
    }
}
