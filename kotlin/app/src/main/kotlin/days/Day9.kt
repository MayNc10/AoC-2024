package days.day9

import java.io.File

import kotlin.math.* 

fun day9() {
    val disk = File("../../input/day9.txt").readText().toCharArray().toList()
    part1(disk)
    part2(disk)
}

fun part1(disk: List<Char>) {
    var count_off_end = 0
    var file_take_from = (disk.size / 2) * 2
    var sum: Long = 0
    var disk_pos = 0
    disk@ for ((pos, char) in disk.withIndex()) {
        val count = char.toString().toInt(); // inefficient but whatever
        if (pos % 2 == 0) {
            // file
            val id = pos / 2
            val num_inc = if (pos == file_take_from) {
                count - count_off_end
            } else {
                count
            };
            for (idx in 0..<num_inc) {
                //println("Adding to sum $disk_pos * $id")
                sum += disk_pos * id
                disk_pos++
            }
            if (pos == file_take_from) {
                break;
            }
        }
        else {
            // free space
            // start taking idxs off end
            var left_to_take = count;
            while (left_to_take > 0) {
                val num_at_end = disk[file_take_from].toString().toInt();
                while (left_to_take > 0 && num_at_end - count_off_end > 0) {
                    val id = file_take_from / 2
                    //println("Adding to sum $disk_pos * $id, taking from idx $file_take_from with ${num_at_end - count_off_end} chars left")
                    sum += disk_pos * id
                    disk_pos++
                    count_off_end++
                    left_to_take--
                }
                // move to next file if finished with this one
                if (num_at_end - count_off_end == 0) {
                    // check if we just consumed the whole list
                    if (file_take_from - 1 == pos) {
                        break@disk;
                    }

                    file_take_from -= 2
                    count_off_end = 0
                }
            }

        }
    }
    println(sum);
}


fun part2(disk: List<Char>) {
    var cleared: MutableSet<Int> = mutableSetOf()
    var sum: Long = 0
    var disk_pos = 0
    for ((pos, char) in disk.withIndex()) {
        val count = char.toString().toInt(); // inefficient but whatever
        if (cleared.contains(pos)) { 
            disk_pos += count
            continue
        }
        if (pos % 2 == 0) {
            // file
            val id = pos / 2
            for (idx in 0..<count) {
                //println("Adding to sum $disk_pos * $id")
                sum += disk_pos * id
                disk_pos++
            }
        }
        else {
            // search reverse through the list
            var amount_left = count
            for (idx in 0..<(disk.size / 2)) {
                val move_pos = disk.size - idx * 2 - (2 - disk.size % 2)
                val count_at_pos = disk[move_pos].toString().toInt()
                // could do a better job of filtering
                if (count_at_pos > amount_left || cleared.contains(move_pos) || move_pos <= pos) {
                    continue
                }
                //println("Move whole file located at ${move_pos} with id ${disk.size / 2 - idx}")
                cleared.add(move_pos)
                repeat(count_at_pos) {
                    //println("Adding to sum free $disk_pos * ${disk.size / 2 - idx}")
                    sum += disk_pos * (disk.size / 2 - idx)
                    disk_pos++
                }
                amount_left -= count_at_pos
                if (amount_left == 0) { break }
            }
            disk_pos += amount_left
        }
    }
    println(sum)
}

