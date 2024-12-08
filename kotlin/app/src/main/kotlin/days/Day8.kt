package days.day8

import java.io.File

import kotlin.math.*
import days.day8.antinodesHarmonic

fun day8() {
    val chars = File("../../input/day8.txt").readText().lines().map { s -> s.toCharArray().toList() } 
    part1(chars)
    part2(chars)
}

fun part1(chars: List<List<Char>>) {
    val nodes = nodes(chars)
    val anodes = nodes.map{(_, poses) -> 
        val cart = cartesianProduct(poses, poses).filter { (p1, p2) -> p1 != p2 }
        val anodes = cart.map {(p1, p2) -> antinodes(p1, p2) } 
        anodes
    }.flatten()
    .map{(p1, p2) -> listOf(p1, p2)}
    .flatten()
    .distinct()
    .filter { p -> inBounds(p, chars.size, chars[0].size) }

    println(anodes.size)
}

fun part2(chars: List<List<Char>>) {
    val nodes = nodes(chars)
    val anodes = nodes.flatMap{(_, poses) -> 
        val cart = cartesianProduct(poses, poses).filter { (p1, p2) -> p1 != p2 }
        val anodes = cart.map {(p1, p2) -> antinodesHarmonic(p1, p2, chars.size, chars[0].size) } 
        anodes
    }.flatten()
    .distinct()

    println(anodes.size)
}

fun nodes(chars: List<List<Char>>): HashMap<Char, List<Pair<Int, Int>>> {
    var amap: HashMap<Char, List<Pair<Int, Int>>>  = HashMap()
    val poses = chars.map { l -> l.withIndex() }.withIndex().map { (ridx, l) -> l.map { (cidx, v) -> Pair(v, Pair(ridx, cidx)) } }.flatten()
    for (char in chars.flatten().distinct()) {
        if (char == '.') { continue }
        amap.put(char, poses.filter { (c,_) -> c == char }.map { (_, pos) -> pos })
    }
    return amap
}

fun antinodes(a11: Pair<Int, Int>, a21: Pair<Int, Int>): Pair<Pair<Int, Int>, Pair<Int, Int>> {
    val (a1, a2) = if (a21.second > a11.second) {Pair(a11, a21)} else {Pair(a11, a21)}
    val slope = slope(a1, a2)
    return Pair(add(a2, slope), sub(a1, slope) )
}

fun antinodesHarmonic(a11: Pair<Int, Int>, a21: Pair<Int, Int>, rlim: Int, clim: Int): MutableList<Pair<Int, Int>> {
    var (a1, a2) = if (a21.second > a11.second) {Pair(a11, a21)} else {Pair(a11, a21)}
    val slope = slope(a1, a2)
    var anodes: MutableList<Pair<Int, Int>> = mutableListOf()
    while ( inBounds(a1, rlim, clim) ) {
        anodes.add(a1)
        a1 = sub(a1, slope)
    }
    while ( inBounds(a2, rlim, clim) ) {
        anodes.add(a2)
        a2 = add(a2, slope)
    }
    return anodes
}

fun inBounds(p: Pair<Int, Int>, rlim: Int, clim: Int): Boolean {
    return p.first >= 0 && p.first < rlim && p.second >= 0 && p.second < clim
}

fun slope(a1: Pair<Int, Int>, a2: Pair<Int, Int>): Pair<Int, Int> {
    return Pair((a2.first - a1.first), (a2.second - a1.second))
}

fun add(p1: Pair<Int, Int>, p2: Pair<Int, Int>): Pair<Int, Int> {
    return Pair(p1.first + p2.first, p1.second + p2.second)
}

fun sub(p1: Pair<Int, Int>, p2: Pair<Int, Int>): Pair<Int, Int> {
    return Pair(p1.first - p2.first, p1.second - p2.second)
}

fun<T> cartesianProduct(l1: List<T>, l2: List<T>): List<Pair<T, T>> {
    return l1.flatMap {i1 -> l2.map {i2 -> Pair(i1, i2)} }
}