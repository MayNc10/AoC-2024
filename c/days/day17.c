#include "day17.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "utils.h"

typedef struct CPU CPU;
struct CPU {
    long a;
    long b;
    long c;
    int ip;
    bool did_print;
    // only used for p2
    int print_idx;
};

#define COMBO_LENGTH 010000

long get_combo(CPU* cpu, int operand) {
    if (operand <= 3) return operand;
    if (operand == 4) return cpu->a;
    if (operand == 5) return cpu->b;
    if (operand == 6) return cpu->c; 
    return -1;
}

inline void adv(CPU* cpu, int operand) {
    cpu->a = cpu->a / (1 << get_combo(cpu, operand));
}
inline void bxl(CPU* cpu, int operand) {
    cpu->b = cpu->b ^ operand;
}
inline void bst(CPU* cpu, int operand) {
    cpu->b = get_combo(cpu, operand) % 8;
}
bool jnz(CPU* cpu, int operand) {
    if (cpu->a != 0) cpu->ip = operand;
    return cpu->a != 0;
}
inline void bxc(CPU* cpu, int operand) {
    cpu->b = cpu->b ^ cpu->c;
}
inline void out(CPU* cpu, int operand) {
    if (cpu->did_print) printf(",");
    printf("%ld", get_combo(cpu, operand) % 8);
    cpu->did_print = true;
}
inline void bdv(CPU* cpu, int operand) {
    cpu->b = cpu->a / (1 << get_combo(cpu, operand));
}
inline void cdv(CPU* cpu, int operand) {
    cpu->c = cpu->a / (1 << get_combo(cpu, operand));
}

long out2(CPU* cpu, int operand, char* program) {
    long output = get_combo(cpu, operand) % 8;
    cpu->print_idx++;
    return output;
}


void step_cpu(CPU* cpu, char* program) {
    char opcode = program[cpu->ip * 2];
    int operand = program[(cpu->ip + 1) * 2] - '0'; // get the number value

    // could use function array but whatever
    switch (opcode) {
        case '0':
            adv(cpu, operand);
        break;
        case '1':
            bxl(cpu, operand);
        break;
        case '2':
            bst(cpu, operand);
        break;
        case '3':
            if (jnz(cpu, operand)) return;
        break;
        case '4':
            bxc(cpu, operand);
        break;
        case '5':
            out(cpu, operand);
        break;
        case '6':
            bdv(cpu, operand);
        break;
        case '7':
            cdv(cpu, operand);
        break;
    }
    cpu->ip += 2;
}

long step_cpu_p2(CPU* cpu, char* program) {
    char opcode = program[cpu->ip * 2];
    int operand = program[(cpu->ip + 1) * 2] - '0'; // get the number value

    // could use function array but whatever
    switch (opcode) {
        case '0':
            adv(cpu, operand);
            break;
        case '1':
            bxl(cpu, operand);
            break;
        case '2':
            bst(cpu, operand);
            break;
        case '3':
            if (jnz(cpu, operand)) {
                return -1;
            }
            break;
        case '4':
            bxc(cpu, operand);
            break;
        case '5':
            cpu->ip += 2;
            return out2(cpu, operand, program);
            break;
        case '6':
            bdv(cpu, operand);
            break;
        case '7':
            cdv(cpu, operand);
            break;
    }
    cpu->ip += 2;
    return -1;
}  

int gen_combos(char* program, int goal, long* arr) {
    int idx = 0;
    CPU cpu = { .b = 0, .c = 0, .ip = 0, .did_print = false, .print_idx = 0};
    for (long i = 0; i < COMBO_LENGTH; i++) {
        cpu.a = i;
        cpu.ip = 0;
        cpu.print_idx = 0;
        do {
            int output = step_cpu_p2(&cpu, program);
            if (output != -1) {
                if (output == goal) {
                    arr[idx] = i;
                    idx++;
                }
            }
        } while (cpu.ip != 0 && cpu.ip * 2 < strlen(program));
    }

    return idx;
}

long* recurse_areg(long* possible, int count, char* program, int step, int* out_count) {
    if (step * 2 >= strlen(program)) return possible;
    int goal_output = program[step * 2] - '0';

    // which combos produce the needed output?
    long* a_arr = malloc(01000 * sizeof(long));
    int idx = gen_combos(program, goal_output, a_arr);
    // combine these as with the past strings
    long* next_arr = malloc(idx * count * sizeof(long));
    int num_next = 0;
    for (int a_idx = 0; a_idx < idx; a_idx++) {
        long lsb_a = a_arr[a_idx] & 077;
        //printf("initial a was %o, reduced is %o\n", a_arr[a_idx], lsb_a);
        for (int pre_idx = 0; pre_idx < count; pre_idx++) {
            int shift = 3 * step ;
            long lsb_pre = possible[pre_idx] >> shift;
            //printf("pre a was %o, shifted by %d, reduced is %o\n", possible[pre_idx], 3 * (step - 1) + 1, lsb_pre);
            if (lsb_pre == lsb_a) {
                long continuation = possible[pre_idx] | (a_arr[a_idx] << shift);
                //printf("pre was %lo, tack on was %lo, shifted tack was %lo, continuing with %lo\n", 
                //    possible[pre_idx], a_arr[a_idx], a_arr[a_idx] << shift, continuation);
                next_arr[num_next] = continuation;
                num_next++;
            }
        }
    }

    // realloc next array
    long* temp = malloc(num_next * sizeof(long));
    memcpy(temp, next_arr, num_next * sizeof(long));
    free(next_arr);
    next_arr = temp;
    free(a_arr);

    // we would recurse here, but we won't for now
    *out_count = num_next;
    //printf("num a at step %d: %d\n", step, num_next);
    return recurse_areg(next_arr, num_next, program, step + 1, out_count);
}


// out arr should be initilized outside of this call as an 8-long array
int build_areg(char* program, int step, long initial, long* out_arr, long* options, int num_options) {
    int idx = 0;
    // not possible to get more than 8 (i think)
    
    // first two of the continuation and last two of the option have to match
    // so step one needs shift down by 1, step 2 needs shift down by 2, etc
    // all those shifts are in terms of octal digits, so we need shift by three times that
    long push_down = initial >> (step * 3);
    for (int i = 0; i < num_options; i++) {
        long masked_option = options[i] & ((COMBO_LENGTH >> 3) - 1);
        if (masked_option == push_down) {
            long out = initial | (options[i] << (step * 3));
            //printf("pushed %lo down to %lo, compared with option %lo masked to %lo, creating output %lo\n", 
            //    initial, push_down, options[i], masked_option, out);

            out_arr[idx] = out;
            idx += 1;
        }
    }

    return idx;
}

void p2(char* program) {
    long* initial_a = malloc(01000 * sizeof(long));
    int init_goal = program[0] - '0';
    int possible = gen_combos(program, init_goal, initial_a);

    /*
    int num_a = 0;
    long* full_a = recurse_areg(initial_a, possible, program, 1, &num_a);
    printf("num full a: %d\n", num_a);
    */

    // build a list
    long* a_list = initial_a;
    int num_a = possible;
    int step = 1;
    int max_step = strlen(program) / 2;
    while (step <= max_step) { // strlen(program) / 2
        printf("step: %d\n", step);
        long* next_list = malloc(8 * num_a * sizeof(long));
        int num_next = 0;
        long* continuations = malloc(COMBO_LENGTH * sizeof(long));
        int num_cont = gen_combos(program, program[step * 2] - '0', continuations);
        //printf("generated %d 3-octal combos for goal %c\n", num_cont, program[step * 2]);

        for (int i = 0; i < num_a; i++) {
            long possible_add[8];
            int num_right = build_areg(program, step, a_list[i], possible_add, continuations, num_cont);
            for (int right_a = 0; right_a < num_right; right_a++) {
                next_list[num_next] = possible_add[right_a];
                num_next += 1;
            }
        }
        num_a = num_next;
        free(a_list);
        a_list = next_list;
        step++;
    }
    printf("created alist with %d elements\n", num_a);

    // ensure they all pass
    // need to put cpu init back
    CPU cpu = { .b = 0, .c = 0, .ip = 0, .did_print = false, .print_idx = 0};
    for (int i = 0; i < num_a; i++) {
        cpu.a = a_list[i];
        cpu.b = 0;
        cpu.c = 0;
        cpu.ip = 0;
        cpu.print_idx = 0;

        int prog_idx = 0;
        bool failed = false;
        while (cpu.ip * 2 < strlen(program) && prog_idx < max_step) {
            int output = step_cpu_p2(&cpu, program);
            if (output != -1) {
                if (prog_idx * 2 >= strlen(program) || output != program[prog_idx * 2] - '0') {
                    printf("uh oh! %lo (octal) doesn't pass the program, output %d when %c (idx %d) was expected\n", 
                        a_list[i], output, program[prog_idx * 2], prog_idx * 2);
                    failed = true;
                    // flag failure
                    a_list[i] = 0;
                    break;
                }
                prog_idx += 1;
            }
        } 
        if (prog_idx != max_step) failed = true;
        if (!failed) {
            printf("SUCCESS ON %lo (octal)\n", a_list[i]);
        }
    }
    long lowest = a_list[0];
    for (int i = 1; i < num_a; i++) {
        if (a_list[i] == 0) continue;
        if (lowest == 0) lowest = a_list[i];
        if (lowest > a_list[i]) lowest = a_list[i]; 
    }
    printf("LOWEST: %ld\n", lowest);

}

void day17() {
    char* buffer = NULL;
    int size = file_to_string("../input/day17.txt", &buffer);
    int num_lines = 0;
    CPU cpu = { .ip = 0, .did_print = false, .print_idx = 0};
    char** lines = split_lines(buffer, size, &num_lines);
    cpu.a = atol(&lines[0][12]);
    cpu.b = atol(&lines[1][12]);
    cpu.c = atol(&lines[2][12]);
    
    char* program = &lines[4][9];
    while (cpu.ip * 2 < strlen(program)) {
        step_cpu(&cpu, program);
    }
    printf("\n");
    
    p2(program);

    // critical notes from thinking
    // at the end of printing our bytes, there's a jump instr
    // we need to fail that after a certain amount of loops
    // during each loop, a is bitshifted 3 times
    // so we know to not reach the end after 15 loops but reach it after 16, 
    // a has to be between 3 * 15 and 3 * 16 bits large. 
    // that's a lot of possible values (and way more than an int), so we need to be cleverer
    
    // wait
    // even bigger breakthrough
    // the steps are generally
    // set b to least 3 bits of a, mask with 7
    // set c to a / b (so could do nothing or at max bitshift a by 3)
    // bitshift a by 3, so resetting for next loop
    // set b to an xor mask of b and c
    // set b to an xor mask of b and 7
    // output b
    // loop

    // i haven't quite figured it out, but i think i know the direction
    // essentialy, the fact that we only print the least 3 bits of b at the end
    // and always initialize b to be the lowest three bits of a at the start
    // means that we only need to look at a certain number of bits for a at each step
    // i think its 6, possibly? it's definitely low
    // also, the other values don't carry over, just a
    // so the general plan is:
    // (somehow) compute at runtime the number of bits of a that 'matter' (and assert b and c don't carry over)
    // for each different output of b, figure out what inputs for the lower bits of a make it work
    // then figure out which of those combinations work with each other, since we only lose 3 bits from a each time


    free(buffer);
}