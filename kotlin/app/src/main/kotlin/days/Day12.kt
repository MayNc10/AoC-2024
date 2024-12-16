package days.day12

import java.io.File

fun day12() {
    val plants = File("../../input/day12.txt").readText().lines().map { s -> s.toCharArray().toList() }
    part1(plants)
    part2(plants)
}

fun part1(plants: List<List<Char>>) {
    var sum = 0
    for ((ridx, row) in plants.withIndex()) {
        for ((cidx, plant) in row.withIndex()) {
            // we can make this more efficient by caching areas but this works
            val area = area(plants, ridx, cidx)
            var perimeter = 0
            if (oob(plants, ridx - 1, cidx) || plants[ridx - 1][cidx] != plant) {
                perimeter += 1
            }
            if (oob(plants, ridx + 1, cidx) || plants[ridx + 1][cidx] != plant) {
                perimeter += 1
            }
            if (oob(plants, ridx, cidx - 1) || plants[ridx][cidx - 1] != plant) {
                perimeter += 1
            }
            if (oob(plants, ridx, cidx + 1) || plants[ridx][cidx + 1] != plant) {
                perimeter += 1
            }
            //println("garden of $plant, with area $area, adding perimeter $perimeter")
            sum += area * perimeter
        }
    }
    println(sum)
}

typealias Edge = Pair<Pair<Int, Int>, Pair<Int, Int>>

fun part2(plants: List<List<Char>>) {
    // build regions
    val regions: MutableSet<Set<Pair<Int, Int>>> = mutableSetOf() 
    for (row in 0..<plants.size) {
        for (col in 0..<plants[0].size) {
            if (!regions.any{s -> s.contains(Pair(row, col))} ) {
                val mset: MutableSet<Pair<Int, Int>> = mutableSetOf()
                area_recurse(plants, row, col, mset, plants[row][col])
                regions.add(mset.toSet())
            }
        }   
    }

    var sum = 0
    for (region in regions) {
        val edge_set: MutableSet<Edge> = mutableSetOf()
        var lines = 0
        for ((row, col) in region) {
            val plant = plants[row][col]
            val potential_line_list: MutableList<Edge> = mutableListOf()
            if (oob(plants, row - 1, col) || plants[row - 1][col] != plant) {
                potential_line_list.add(Pair(Pair(-1, 0), Pair(row, col)))
            }
            if (oob(plants, row + 1, col) || plants[row + 1][col] != plant) {
                potential_line_list.add(Pair(Pair(1, 0), Pair(row, col)))
            }
            if (oob(plants, row, col - 1) || plants[row][col - 1] != plant) {
                potential_line_list.add(Pair(Pair(0, -1), Pair(row, col)))
            }
            if (oob(plants, row, col + 1) || plants[row][col + 1] != plant) {
                potential_line_list.add(Pair(Pair(0, 1), Pair(row, col)))
            }

            val line_list = potential_line_list.filter { edge -> !edge_set.contains(edge) }
            for ((dir, start) in line_list) {
                lines += 1

                var point = start
                if (dir.first != 0) {
                    //print("Horizontal ")

                    // step left
                    while (still_edge(plants, plant, point, dir)) 
                    {
                        edge_set.add(Pair(dir, point))
                        point = Pair(point.first, point.second - 1)
                    }
                    // step right
                    point = start
                    while (still_edge(plants, plant, point, dir)) 
                    {
                        edge_set.add(Pair(dir, point))
                        point = Pair(point.first, point.second + 1)
                    }
                }
                else {
                    //print("Vertical ")

                    // step up
                    while (still_edge(plants, plant, point, dir)) 
                    {
                        edge_set.add(Pair(dir, point))
                        point = Pair(point.first - 1, point.second)
                    }
                    // step right
                    point = start
                    while (still_edge(plants, plant, point, dir)) {
                        edge_set.add(Pair(dir, point))
                        point = Pair(point.first + 1, point.second)
                    }
                }
                //println("point at $start")
            }
        }
        //println("Edge points:")
        //edge_set.forEach {e -> println("$e")}
        //println("Area: ${region.size}, lines: ${lines}")
        sum += lines * region.size
    }
    println(sum)
}

fun still_edge(plants: List<List<Char>>, plant: Char, point: Pair<Int, Int>, dir: Pair<Int, Int>): Boolean {
    return !oob(plants, point.first, point.second) 
        && plants[point.first][point.second] == plant
        && ( oob(plants, point.first + dir.first, point.second + dir.second) 
        || plants[point.first + dir.first][point.second + dir.second] != plant)
}

fun area(plants: List<List<Char>>, row: Int, col: Int): Int {
    // flood fill
    val key = plants[row][col]
    val set: MutableSet<Pair<Int, Int>> = mutableSetOf()
    area_recurse(plants, row, col, set, key)
    return set.size
}

fun area_recurse(plants: List<List<Char>>, row: Int, col: Int, set: MutableSet<Pair<Int, Int>>, key: Char) {
    val p = Pair(row, col)
    if (set.contains(p)) { return }
    if (oob(plants, row, col)) { return }
    if (plants[row][col] != key) { return }
    set.add(p)
    area_recurse(plants, row + 1, col, set, key)
    area_recurse(plants, row - 1, col, set, key)
    area_recurse(plants, row , col + 1, set, key)
    area_recurse(plants, row , col - 1, set, key)
}

fun oob(plants: List<List<Char>>, row: Int, col: Int): Boolean {
    return row < 0 || row >= plants.size || col < 0 || col >= plants[0].size
} 

