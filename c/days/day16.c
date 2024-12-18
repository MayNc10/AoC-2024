#include "day16.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "utils.h"

struct Point {
    int row;
    int col;
};
typedef struct Point Point;

Point add(Point p1, Point p2) {
    Point p = {.row = p1.row + p2.row, .col = p1.col + p2.col};
    return p;
}

bool eq(Point p1, Point p2) {
    return p1.row == p2.row && p1.col == p2.col;
}

typedef Point Dir;

Dir turn_right(Dir dir) {
    Dir right_dir = { .row = dir.col, .col = dir.row * -1 };
    return right_dir;
}

Dir turn_left(Dir dir) {
    Dir left_dir = { .row = dir.col * -1, .col = dir.row };
    return left_dir;
}

int enum_from_dir(Dir dir) {
    if (dir.col == 0) {
        return dir.row + 2;
    }
    else {
        return dir.col + 1;
    }
}

struct Loc {
    Point pos;
    Dir dir;
};
typedef struct Loc Loc;

bool eq_loc(Loc l1, Loc l2) {
    return eq(l1.dir, l2.dir) && eq(l1.pos, l2.pos);
}

struct LocWithStats {
    Loc loc;
    int score;
};
typedef struct LocWithStats LocWithStats;

// yes, i know about heaps. don't @ me
typedef struct LinkedQueueNode LinkedQueueNode;
struct LinkedQueueNode {
    LocWithStats data;
    LinkedQueueNode* next;
    // extra for p2
    int parent_index;
};

// please no null heads!
LinkedQueueNode take_head(LinkedQueueNode** head) {
    LinkedQueueNode temp = **head;
    *head = (*head)->next;
    return temp;
}

int heuristic(Loc current, Point end);

LinkedQueueNode* insert_ll(LinkedQueueNode* head, LocWithStats val, Point end, int* length, int parent_idx) {
    LinkedQueueNode* node = malloc(sizeof(LinkedQueueNode));
    node->next = NULL;
    node->data = val;
    node->parent_index = parent_idx;
    // null check
    if (head == NULL) {
        return node;
    }

    // check if top
    if (node->data.score + heuristic(node->data.loc, end) < head->data.score + heuristic(head->data.loc, end)) {
        node->next = head;
        *length += 1;
        return node;
    }

    LinkedQueueNode* current = head;
    while (current->next != NULL) {
        LinkedQueueNode next = *(current->next);

        if (node->data.score != next.data.score && eq_loc(next.data.loc, node->data.loc)) { // node->data.score != current->data.score &&
            //printf("hit duplicates at point (%d, %d)\n", next.data.loc.pos.row, next.data.loc.pos.col);


            // don't add duplicates, as long as one is stictly better
            if (node->data.score < next.data.score) {
                // just replace the score, no need to "swap and drop"
                current->next->data.score = node->data.score;
            }
            // since either way we lose the node here, dealloc it
            //free(node);
            return head;
        }
        //else if (node->data.score == current->data.score && eq_loc(current->data.loc, node->data.loc)) {
        //    printf("duplicate node!\n");
        //}

        
        if (node->data.score + heuristic(node->data.loc, end) < next.data.score + heuristic(next.data.loc, end)) {
            node->next = current->next;
            current->next = node;
            *length += 1;
            return head;
        }
        current = current->next;
    }
    current->next = node;
    *length += 1;
    return head;
}

void dealloc_ll(LinkedQueueNode* head) {
    LinkedQueueNode* temp;
    while (head != NULL) {
        temp = head;
        head = head->next;
        free(temp);
    } 
}

Point get_start(char** map, int rows, int cols) {
    Point p = { .row = 0, .col = 0 };
    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < cols; col++) {
            if (map[row][col] == 'S') { 
                p.row = row;
                p.col = col;
                return p;
            }
        }
    }
    return p;
}

Point get_end(char** map, int rows, int cols) {
    Point p = { .row = 0, .col = 0 };
    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < cols; col++) {
            if (map[row][col] == 'E') { 
                p.row = row;
                p.col = col;
                return p;
            }
        }
    }
    return p;
}

bool in_bounds(Point p, int rows, int cols) {
    return p.row >= 0 && p.row < rows && p.col >= 0 && p.col < cols;
}

int gen_next_moves(char** map, int rows, int cols, Loc current, LocWithStats possible[3]) {
    int idx = 0;
    Point temp;
    // go straight?
    temp = add(current.pos, current.dir);
    if (in_bounds(temp, rows, cols) && map[temp.row][temp.col] != '#') {
        possible[idx].loc.pos = temp; 
        possible[idx].loc.dir = current.dir;
        possible[idx].score = 1;
        idx += 1;
    }
    
    // go left?
    temp = add(current.pos, turn_left(current.dir));
    if (in_bounds(temp, rows, cols) && map[temp.row][temp.col] != '#') {
        possible[idx].loc.pos = temp; 
        possible[idx].loc.dir = turn_left(current.dir);
        possible[idx].score = 1001;
        idx += 1;
    }
    // go right?
    temp = add(current.pos, turn_right(current.dir));
    if (in_bounds(temp, rows, cols) && map[temp.row][temp.col] != '#') {
        possible[idx].loc.pos = temp; 
        possible[idx].loc.dir = turn_right(current.dir);
        possible[idx].score = 1001;
        idx += 1;
    }
    
    return idx;
}

int heuristic(Loc current, Point end) {
    return 0;
    int turning = 0;
    // simple h(x)
    if (current.pos.row != end.row && current.pos.col != end.col) turning = 1000;

    return abs(current.pos.row - end.row) + abs(current.pos.col - end.col) + turning;
}

void solve(char** map, int rows, int cols) {
    Point start = get_start(map, rows, cols);
    Loc start_loc = { .pos = start, .dir = { .row = 0, .col = 1 } };
    Point end = get_end(map, rows, cols);

    int* pos_array = malloc(rows * cols * 4 * sizeof(int));\
    for (int i = 0; i < rows * cols * 4; i++) {
        pos_array[i] = -1;
    }

    // whatever, just make it size 1000 for now
    LinkedQueueNode* past_array = malloc(1000 * sizeof(LinkedQueueNode));
    int past_idx = 0;
    int past_max = 1000;

    // priority queue
    LinkedQueueNode* head = malloc(sizeof(LinkedQueueNode));
    head->next = NULL;
    head->data.loc = start_loc;
    head->data.score = 0;
    head->parent_index = -1;

    int step = 0;
    int len = 1;

    while (!eq(head->data.loc.pos,end)) {
        LinkedQueueNode raw_head = take_head(&head);
        // add head to array
        if (past_idx >= past_max) {
            // realloc
            LinkedQueueNode* new = malloc(2 * past_max * sizeof(LinkedQueueNode));
            memcpy(new, past_array, past_max * sizeof(LinkedQueueNode));
            free(past_array);
            past_array = new;
            past_max *= 2;
        }
        past_array[past_idx] = raw_head;

        LocWithStats new_options[3];
        int num = gen_next_moves(map, rows, cols, raw_head.data.loc, new_options);
        // add new positions to queue
        int base_score = raw_head.data.score;
        for (int i = 0; i < num; i++) {
            new_options[i].score += base_score; 
            int idx = (rows * cols * enum_from_dir(new_options[i].loc.dir)) + new_options[i].loc.pos.row * cols + new_options[i].loc.pos.col;
            if (pos_array[idx] == -1 || pos_array[idx] >= new_options[i].score) {
                pos_array[idx] = new_options[i].score;
                head = insert_ll(head, new_options[i], end, &len, past_idx);
            }
        }

        past_idx++;
    }

    printf("Par t1: %d\n", head->data.score);
    LinkedQueueNode* current = head;
    int idx = 0;
    // make array
    bool* visited_array = calloc(rows * cols, sizeof(Point));

    // follow chain
    int num_tiles = 1;
    current = head;
    while (current != NULL) {
        if (eq(current->data.loc.pos, head->data.loc.pos) && current->data.score == head->data.score) {
            int idx = current->parent_index;
            while (idx != -1) {
                int pos = past_array[idx].data.loc.pos.row * cols + past_array[idx].data.loc.pos.col; 
                if (!visited_array[pos]) {
                    num_tiles += 1;
                    visited_array[pos] = true;
                } 
                
                idx = past_array[idx].parent_index;
            }
        }
        current = current->next;
    }
    printf("Part 2: %d\n", num_tiles);

    dealloc_ll(head);
}

void day16() {
    char* buffer = NULL;
    int size = file_to_string("../input/day16.txt", &buffer);
    int rows, cols;
    char** array = split_lines_square(buffer, size, &cols, &rows);

    solve(array, rows, cols);

    dealloc_lines(&array, rows);
    free(buffer);
}

