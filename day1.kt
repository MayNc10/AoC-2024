fun main() {
    println("Hello, World!")
    val list = listOf("hi", "world", "this", "is", "may")
    do_thing(list)
    
}

fun do_thing(thing: List<String>) {
    thing.forEach { i: String -> println(i) }
}