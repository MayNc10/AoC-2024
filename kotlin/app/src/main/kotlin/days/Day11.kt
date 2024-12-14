package days.day11

import java.io.File
import kotlin.math.*

fun day11() {
    val stones = File("../../input/day11.txt").readText().trim().split("\\s+".toRegex()).map { s -> s.toULong() }
    blink(stones, 25) // part1
    blink(stones, 75)
}


fun blink(stones: List<ULong>, steps: Int) {
    var stones = stones.map { v: ULong -> Pair(v, stones.count{it == v}.toULong()) }.distinct()
    repeat(steps) {
        val new_stones = stones.map { (stone, count) -> stone_transition(stone).map { s -> Pair(s, count) } }.flatten()
        stones = new_stones.map { (s, _) -> s }.distinct().map { s -> Pair(s, new_stones.filter {(s1, _) -> s == s1}.map{(_, c) -> c}.sum()) }
    }
    println(stones.map{ (_, c) -> c}.sum() )
}

fun stone_transition(stone: ULong): List<ULong> {
    if (stone == 0UL) { return listOf(1UL) }
    else if (log10(stone.toDouble()).toULong() % 2UL == 1UL) {
        val s = stone.toString()
        assert(s.length % 2 == 0)
        return listOf(s.substring(0, s.length / 2).toULong(), s.substring(s.length / 2, s.length).toULong())
    }
    else {
        assert(stone * 2024UL > stone)

        return listOf(stone * 2024UL)
    }
}
