package days.day5

import java.io.File

fun day5() {
    val chars = File("../../input/day5.txt").readText().lines()
    var orderings: MutableList<Pair<Int, Int>> = arrayListOf()
    var updates: MutableList<List<Int>> = arrayListOf()
    for (line in chars ) {
        if (line.contains('|')) {
            val split = line.split('|')
            val pair = Pair(split[0].toInt(), split[1].toInt())
            orderings.add(pair)
        }
        else if (line.contains(',')) {
            val split = line.split(',')
            updates.add(split.map { s -> s.toInt() })
        }
    }

    part1(orderings, updates)
    part2(orderings, updates)
}

fun part1(orderings: MutableList<Pair<Int, Int>>, updates: MutableList<List<Int>>) {
    val correct = updates.filter { update -> isCorrect(orderings, update) }
    val sum = correct.map { list -> list[list.size / 2] }.sum()
    println(sum)
}

fun part2(orderings: MutableList<Pair<Int, Int>>, updates: MutableList<List<Int>>) {
    val incorrect = updates.filter { update -> !isCorrect(orderings, update) }
    val sum = incorrect.map { list -> fix(orderings, list)[list.size / 2] }.sum()
    println(sum)
}

fun isCorrect(orderings: MutableList<Pair<Int, Int>>, update: List<Int>): Boolean {
    for ((i, item) in update.withIndex()) {
        val pairs = update.subList(i + 1, update.size).map { it -> orderings.contains(Pair(it, item)) }
        if (pairs.any { it }) { return false }
    }

    return true
}

fun fix(orderings: MutableList<Pair<Int, Int>>, update: List<Int>): List<Int> {
    return update.sortedBy { 
        elem -> update.minus(elem).filter {
            num -> orderings.contains(Pair(num, elem))
        }.size
    }
}