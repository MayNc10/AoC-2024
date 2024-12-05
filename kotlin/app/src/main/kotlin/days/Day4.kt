package days.day4

import java.io.File

fun day4() {
    val chars = File("../../input/day4.txt").readText().lines().map { s -> s.toCharArray().toList() } 
    part1(chars)
    part2(chars)
}

fun part1(chars: List<List<Char>>) {
    var count = 0
    for ((r_idx, row) in chars.withIndex()) {
        for ((c_idx, char) in row.withIndex()) {
            if (char != 'X') { continue }

            // check forward
            if (c_idx < row.size - 3 && row.subList(c_idx, c_idx+4).joinToString(separator="") == "XMAS") {
                count += 1
            }
            // and backward
            if (c_idx >= 3  && row.subList(c_idx-3, c_idx+1).joinToString(separator="")== "SAMX") {
                count += 1
            }
            // and down
            if (r_idx < chars.size - 3 && chars[r_idx + 1][c_idx] == 'M' && chars[r_idx + 2][c_idx] == 'A' && chars[r_idx + 3][c_idx] == 'S') {
                count += 1
            }
            // and up
            if (r_idx >= 3 && chars[r_idx - 1][c_idx] == 'M' && chars[r_idx - 2][c_idx] == 'A' && chars[r_idx - 3][c_idx] == 'S') {
                count += 1
            }
            // and NE
            if (r_idx >= 3 && c_idx < row.size - 3 
                && chars[r_idx - 1][c_idx+1] == 'M' && chars[r_idx - 2][c_idx+2] == 'A' && chars[r_idx - 3][c_idx+3] == 'S') 
            {
                count += 1
            }
            // and SE
            if (r_idx < chars.size - 3 && c_idx < row.size - 3 
                && chars[r_idx + 1][c_idx+1] == 'M' && chars[r_idx + 2][c_idx+2] == 'A' && chars[r_idx + 3][c_idx+3] == 'S') 
            {
                count += 1
            }
            // and NW
            if (r_idx >= 3 && c_idx >= 3
                && chars[r_idx - 1][c_idx-1] == 'M' && chars[r_idx - 2][c_idx-2] == 'A' && chars[r_idx - 3][c_idx-3] == 'S') 
            {
                count += 1
            }
            // and SW
            if (r_idx < chars.size - 3 && c_idx >= 3
                && chars[r_idx + 1][c_idx-1] == 'M' && chars[r_idx + 2][c_idx-2] == 'A' && chars[r_idx + 3][c_idx-3] == 'S') 
            {
                count += 1
            }
        }
    }
    println(count)
}

fun part2(chars: List<List<Char>>) {
    var count = 0
    for ((r_idx, row) in chars.withIndex()) {
        for ((c_idx, char) in row.withIndex()) {
            if (char != 'A') { continue }
            if (r_idx < 1 || r_idx == chars.size - 1 || c_idx < 1 || c_idx == row.size - 1) { continue }

            val c1 = chars[r_idx-1][c_idx-1]
            val c2 = chars[r_idx-1][c_idx+1]
            val c3 = chars[r_idx+1][c_idx-1]
            val c4 = chars[r_idx+1][c_idx+1]

            if (isMas(c1, c4) && isMas(c2, c3)) {
                count += 1
            }        
        }  
    }
    println(count)
}

fun isMas(c1: Char, c2: Char): Boolean {
    return (c1 == 'M' || c1 == 'S') && (c2 == 'M' || c2 == 'S') && (c1 != c2)
}


