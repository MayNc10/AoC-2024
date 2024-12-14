package days.day10

import java.io.File

// search stuff
typealias Pos = Pair<Int, Int>


fun day10() {
    val map = File("../../input/day10.txt").readText().lines().map { s -> s.toCharArray().toList().map(Character::getNumericValue) } 
    part1(map)
    part2(map)
}

fun part1(map: List<List<Int>>) {
    val sum = get_starts(map).map { p -> p1_score(map, p) }.sum()
    println(sum)
}

fun part2(map: List<List<Int>>) {
    val sum = get_starts(map).map { p -> p2_score(map, p) }.sum()
    println(sum)
}

fun get_starts(map: List<List<Int>>): List<Pos> {
    return map.withIndex().map { 
        (ridx: Int, l: List<Int>) -> l.withIndex()
        .filter { (_, i) -> i == 0}
        .map { (lidx, _) -> Pos(ridx, lidx) } 
    }.flatten()
}

fun p1_score(map: List<List<Int>>, start: Pos): Int {
    var pos_list = listOf(start)
    var peak_list: MutableList<Pos> = mutableListOf()
    val down = map.size
    val right = map[0].size
    while (pos_list.size > 0) {
        val new_list = pos_list.map { p -> cardinal_list(p, down, right)
                .filter { p2 -> map[p2.first][p2.second] == map[p.first][p.second] + 1 } }.flatten()
            
        peak_list.addAll(new_list.filter { p -> map[p.first][p.second] == 9 })
        pos_list = new_list.filter { p -> map[p.first][p.second] != 9 }
    }
    return peak_list.distinct().size
}

fun p2_score(map: List<List<Int>>, start: Pos): Int {
    var pos_list = listOf(listOf(start))
    var peak_list: MutableList<List<Pos>> = mutableListOf()
    val down = map.size
    val right = map[0].size
    while (pos_list.size > 0) {
        val new_list = pos_list.map { p -> cardinal_list(p.last(), down, right)
                .filter { p2 -> map[p2.first][p2.second] == map[p.last().first][p.last().second] + 1 }
                .map {e -> p + e}  }.flatten()
            
        peak_list.addAll(new_list.filter { p -> map[p.last().first][p.last().second] == 9 })
        pos_list = new_list.filter { p -> map[p.last().first][p.last().second] != 9 }
    }
    return peak_list.distinct().size
}

fun cardinal_list(pos: Pos, down_bound: Int, right_bound: Int): List<Pos> {
    return listOf(Pos(pos.first - 1, pos.second), Pos(pos.first, pos.second + 1), Pos(pos.first + 1, pos.second), Pos(pos.first, pos.second - 1))
        .filter { p -> p.first >= 0 && p.first < down_bound && p.second >= 0 && p.second < right_bound }
}
