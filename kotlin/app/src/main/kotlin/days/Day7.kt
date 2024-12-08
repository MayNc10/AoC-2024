package days.day7

import java.io.File

import kotlin.math.*

fun day7() {
    val chars = File("../../input/day7.txt").readText().lines()
    val eqs = chars.map { str -> 
        val sides = str.split(':')
        val lhs = sides[0].toLong()
        val rhs = sides[1].trim().split(' ').map { s -> s.toLong() }
        Pair(lhs, rhs)
    }

    part1(eqs)
    part2(eqs)
}

fun part1(eqs: List<Pair<Long, List<Long>>>) {
    val sum = eqs.filter(::isSolvable).map{p -> p.first}.sum()
    println(sum)
}

fun part2(eqs: List<Pair<Long, List<Long>>>) {
    val sum = eqs.filter{eq -> isSolvableWithConcat(eq, true) != null }.map{p -> p.first}.sum()
    println(sum)
}

fun isSolvable(eq: Pair<Long, List<Long>>): Boolean {
    if (eq.second.size == 0) { return false } 
    val lhs = eq.first
    val rhs = eq.second.last()
    if (lhs == rhs) { return true }
    val rest = eq.second.subList(0, eq.second.size - 1)
    //println ("lhs: $lhs, rhs: $rhs, rest: $rest")
    val eqs: MutableList<Pair<Long, List<Long>>> = arrayListOf()
    if (lhs % rhs == 0L) {
        eqs.add(Pair(lhs / rhs, rest))
    }
    if (lhs - rhs > 0L) {
        eqs.add(Pair(lhs - rhs, rest))
    }
    return eqs.any(::isSolvable)
}

fun isSolvableWithConcat(eq: Pair<Long, List<Long>>, fst: Boolean): List<Int>? {
    val lhs = eq.first
    val rhs = eq.second[0]
    if (eq.second.size == 1) {
        if (lhs == rhs) { return listOf() }
        else { return null }
    }

    val rest = eq.second.subList(1, eq.second.size)
    //-println("lhs: $lhs, rhs: $rhs, rest: $rest")
    val rplus = rest.toMutableList()
    rplus[0] += rhs
    val rmul = rest.toMutableList()
    rmul[0] *= rhs

    val rconc = rest.toMutableList()
    val p10 = (10.0).pow( ceil(log10(rconc[0].toDouble() + 1)) ).toLong()
    rconc[0] += (rhs * p10)

    val eqs = arrayListOf(Pair(lhs, rplus), Pair(lhs, rmul), Pair(lhs, rconc))
    val list: List<List<Int>> = eqs.withIndex()
            .map{p -> Pair(p.index, isSolvableWithConcat(p.value, false))}
            .filter { p: Pair<Int, List<Int>?> -> p.second != null }
            .map { p -> p.second!!.plusElement(p.first)}
    return if (list.size > 0) { list.first() } else { null }  
}

