package days.day1

import java.io.File
import kotlin.math.abs

fun day1() {
    val lines = File("../../input/day1.txt").readText().lines().map { s -> s.split("\\s+".toRegex())}
    val list1 = lines.map{s -> s[0].toInt()}.sorted()
    val list2 = lines.map{s -> s[1].toInt()}.sorted()
    part1(list1, list2)
    part2(list1, list2)
}

fun part1(list1: List<Int>, list2: List<Int>) {
    val sum = list1.withIndex().fold(0) { acc, item: IndexedValue<Int> -> acc + abs(item.value - list2[item.index]) }
    println(sum)
}

fun part2(list1: List<Int>, list2: List<Int>) {
    val sum = list1.fold(0) { acc, item: Int -> acc + item * list2.count{it == item}}
    println(sum)
}