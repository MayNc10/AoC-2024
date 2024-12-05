package days.day2

import java.io.File
import kotlin.math.*
fun day2() {
    val lines = File("../../input/day2.txt").readText().lines().map { s -> s.split("\\s+".toRegex()).map { n: String -> n.toInt() } }
    part1(lines)
    part2(lines)
}

fun part1(list: List<List<Int>>) {
    println(list.filter { l -> isSafe(l) }.size)
}

fun part2(list: List<List<Int>>) {
    println(list.filter { l -> (0..<l.size).map{idx -> 
        val newList = l.toMutableList()
        newList.removeAt(idx)
        newList    
    }.any(::isSafe)  }.size)
}

fun isSafe(list: List<Int>): Boolean {
    val windows = list.windowed(2)
    for ((n1, n2) in windows) {
        val diff = n1 - n2
        if (abs(diff) < 1 || abs(diff) > 3) {
            return false
        }
        if (diff.sign != (windows[0][0] - windows[0][1]).sign ) {
            return false
        }
    }
    return true
}

