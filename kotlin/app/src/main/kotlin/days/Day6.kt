package days.day6

import java.io.File
import days.day6.rightTurn
import days.day6.doesPathLoopOther
import days.day6.doesPathLoop


fun day6() {
    val chars = File("../../input/day6.txt").readText().lines().map { s -> s.toCharArray().toList() } 
    part1(chars)
    part2(chars)
}

fun part1(chars: List<List<Char>>) {
    val guardPos = guardPos(chars)
    val path = computePath(chars, guardPos)
    //println("path = $path")
    println(path.distinct().size)
}

fun part2(chars: List<List<Char>>) {
    var mutmap = chars.map { l -> l.toMutableList() }.toMutableList()
    val guardPos = guardPos(chars)
    var num_loops = 0
    val path = computePath(chars, guardPos)
    val distPath = path.distinct().filter{loc -> loc != guardPos}

    for ((idx, loc) in distPath.withIndex()) {
        //println("${idx.toFloat() / (distPath.size - 1).toFloat() * 100.0 }%")
        mutmap[loc.first][loc.second] = '#'
        val loop = doesPathLoop(mutmap, guardPos)
        if (loop) {
            num_loops += 1
            //println(loc)
        }
        if (loop != doesPathLoopOther(mutmap, guardPos)) {
            //println(loc)
            //println(computePath(mutmap, guardPos))
        }   
        //print("check says $loop, ")
        //computePath(mutmap, guardPos)

        
        mutmap[loc.first][loc.second] = '.'
    }

    println(num_loops)
}

fun guardPos(chars: List<List<Char>>): Pair<Int, Int> {
    return chars.withIndex()
        .map { l -> Pair(l.index, l.value.withIndex().filter {(_, c) -> c == '^'}) }
        .filter { (_, p) -> p.size > 0 }
        .map {(ridx, l) -> Pair(ridx, l[0].index)}[0]
}

fun computePath(chars: List<List<Char>>, start: Pair<Int, Int>): MutableList<Pair<Int, Int>> {
    var pos = start
    var dir = Pair(-1, 0)
    val path: MutableList<Pair<Int, Int>> = mutableListOf()
    val pairs:  MutableList<Pair<Pair<Int, Int>, Pair<Int, Int>>> = mutableListOf()
    while (true) {
        if (pairs.contains(Pair(pos, dir))) {
            println("found loop!")
            return path
        }
        path.add(pos)
        pairs.add(Pair(pos, dir))
        //println(pos)
        val next = add(pos, dir)
        if (next.first < 0 || next.first >= chars.size || next.second < 0 || next.second >= chars[0].size) {
            break
        }
        if (chars[next.first][next.second] == '#') {
            dir = rightTurn(dir)
            continue
        }
        // recalculate incase dir change
        pos = add(pos, dir)
    }
    return path
}

fun doesPathLoop(chars: List<List<Char>>, start: Pair<Int, Int>): Boolean {
    var pos = start
    var dir = Pair(-1, 0)
    val map: MutableSet<Pair<Pair<Int, Int>, Pair<Int, Int>>> = mutableSetOf()
    while (!map.contains(Pair(pos, dir))) {
        val next = add(pos, dir)
        if (next.first < 0 || next.first >= chars.size || next.second < 0 || next.second >= chars[0].size) {
            break
        }
        map.add(Pair(pos, dir))
        if (chars[next.first][next.second] == '#') {
            dir = rightTurn(dir)
            continue
        }

        // recalculate incase dir change
        pos = add(pos, dir)
    }
    return map.contains(Pair(pos, dir))
}
fun doesPathLoopOther(chars: List<List<Char>>, start: Pair<Int, Int>): Boolean {
    var pos = start
    var dir = Pair(-1, 0)
    val map: MutableSet<Pair<Pair<Int, Int>, Pair<Int, Int>>> = mutableSetOf()
    while (!map.contains(Pair(pos, dir))) {
        val next = add(pos, dir)
        if (next.first < 0 || next.first >= chars.size || next.second < 0 || next.second >= chars[0].size) {
            break
        }
        
        if (chars[next.first][next.second] == '#') {
            dir = rightTurn(dir)
            continue
        }
        map.add(Pair(pos, dir)) // add is here now

        // recalculate incase dir change
        pos = add(pos, dir)
    }
    return map.contains(Pair(pos, dir))
}

fun rightTurn(dir: Pair<Int, Int>): Pair<Int, Int> {
    return Pair(dir.second, dir.first * -1)
}

fun add(p1: Pair<Int, Int>, p2: Pair<Int, Int>): Pair<Int, Int> {
    return Pair(p1.first + p2.first, p1.second + p2.second)
}

fun sub(p1: Pair<Int, Int>, p2: Pair<Int, Int>): Pair<Int, Int> {
    return Pair(p1.first - p2.first, p1.second - p2.second)
}