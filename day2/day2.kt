import java.io.File
import kotlin.math.abs

fun main() {
    val lines = File("input/day1.txt").readText().lines()

}

fun part1(list1: List<Int>, list2: List<Int>) {
    val sum = list1.withIndex().fold(0) { acc, item: IndexedValue<Int> -> acc + abs(item.value - list2[item.index]) }
    println(sum)
}