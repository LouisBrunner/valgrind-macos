/********************************************************************************
**  Program: concord.c
**  By: Nick Nethercote, 36448.  Any code taken from elsewhere as noted.
**  For: 433-253 assignment 3.
**  
**  Program description:  This program is a tool for finding specific 
**  occurrences of words in a text;  it can count the number of times a single
**  word appears, or list the lines that a word, or multiple words, all appear
**  on.  See the project specification for more detail.
**  	The primary data structure used is a static hash table, of fixed size.
**  Any collisions of words hashing to the same position in the table are
**  dealt with via separate chaining.  Also, for each word, there is a 
**  subsidiary linked list containing the line numbers that the word appears on.
**  Thus there are linked lists within linked lists.
**  	I have implemented the entire program within one file, partly because
**  there isn't a great deal of code, and partly because I haven't yet done
**  433-252, and thus don't know a great deal about .h files, makefiles, etc.
*/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 1
#define FALSE 0
#define MAX_WORD_LENGTH 100 
#define DUMMY_WORD_LENGTH 2
#define TABLE_SIZE 997 
#define BEFORE_WORD 1
#define IN_WORD 2
#define AFTER_WORD 3
#define HASH_CONSTANT 256
#define ARGS_NUMBER 1

typedef struct word_node Word_Node;
typedef struct line_node Line_Node;
typedef struct word_info Word_Info;
typedef struct arg_node Arg_Node;

/*  Linked list node for storing each word */
struct word_node {
    char *word;		    /* The actual word */
    int number;		    /* The number of occurrences */	
    Line_Node *line_list;   /* Points to the linked list of line numbers */
    Line_Node *last_line;   /* Points to the last line node, for easy append */
    Word_Node *next_word;   /* Next node in list */
};

/*  Subsidiary linked list node for storing line numbers */
struct line_node {
    int line;
    Line_Node *next_line;
};

/*  Structure used when reading each word, and it line number, from file. */
struct word_info {
    char word[MAX_WORD_LENGTH];
    int line;
};

/*  Linked list node used for holding multiple arguments from the program's
**  internal command line.  Also, can point to a list of line numbers;  this
**  is used when displaying line numbers.  
*/ 
struct arg_node {
    char *word;
    Line_Node *line_list;
    Arg_Node *next_arg;
};

int        hash(char *word);
void      *create(int mem_size);
void       init_hash_table(char *file_name, Word_Node *table[]);
int        get_word(Word_Info *data, int line, FILE *file_ptr);
void       insert(char *inword, int in_line, Word_Node *table[]);
Word_Node *new_word_node(char *inword, int in_line);
Line_Node *add_existing(Line_Node *curr, int in_line);
void       interact(Word_Node *table[]);
Arg_Node  *place_args_in_list(char command[]);
Arg_Node  *append(char *word, Arg_Node *head);
void       count(Arg_Node *head, Word_Node *table[]);
void       list_lines(Arg_Node *head, Word_Node *table[]);
void       intersection(Arg_Node *head);
void       intersect_array(int master[], int size, Arg_Node *arg_head);
void       kill_arg_list(Arg_Node *head);

int main(int argc, char *argv[])
{
    /* The actual hash table, a fixed-size array of pointers to word nodes */
    Word_Node *table[TABLE_SIZE];

    /* Checking command line input for one file name */
    if (argc != ARGS_NUMBER + 1) {
	fprintf(stderr, "%s requires %d argument\n", argv[0], ARGS_NUMBER); 
	exit(EXIT_FAILURE);
    }

    init_hash_table(argv[1], table);
    interact(table);

    /* Nb:  I am not freeing the dynamic memory in the hash table, having been
    ** told this is not necessary. */
    return 0;
}

/* General dynamic allocation function that allocates and then checks. */
void *create(int mem_size)
{
    void *dyn_block;

    dyn_block = malloc(mem_size);
    if (!(dyn_block)) {
        fprintf(stderr, "Couldn't allocate enough memory to continue.\n");
        exit(EXIT_FAILURE);
    }

    return dyn_block;
}

/* Function returns a hash value on a word.  Almost identical to the hash
** function presented in Sedgewick.
*/
int hash(char *word)
{
    int hash_value = 0;

    for ( ; *word; word++)
        hash_value = (HASH_CONSTANT * hash_value + *word) % TABLE_SIZE;

    return hash_value;
}

/* Function builds the hash table from the given file. */
void init_hash_table(char *file_name, Word_Node *table[])
{
    FILE *file_ptr;
    Word_Info *data;
    int line = 1, i;

    /* Structure used when reading in words and line numbers. */
    data = (Word_Info *) create(sizeof(Word_Info));

    /* Initialise entire table to NULL. */
    for (i = 0; i < TABLE_SIZE; i++)
        table[i] = NULL;

    /* Open file, check it. */
    file_ptr = fopen(file_name, "r");
    if (!(file_ptr)) {
        fprintf(stderr, "Couldn't open '%s'.\n", file_name);
        exit(EXIT_FAILURE);
    }

    /*  'Get' the words and lines one at a time from the file, and insert them
    ** into the table one at a time. */
    while ((line = get_word(data, line, file_ptr)) != EOF)
        insert(data->word, data->line, table);

    free(data);
    fclose(file_ptr);
}

/* Function reads the next word, and it's line number, and places them in the 
** structure 'data', via a pointer.
*/
int get_word(Word_Info *data, int line, FILE *file_ptr)
{
    int index = 0, pos = BEFORE_WORD;

    /* Only alphabetic characters are read, apostrophes are ignored, and other
    ** characters are considered separators.  'pos' helps keep track whether
    ** the current file position is inside a word or between words.
    */
    while ((data->word[index] = tolower(fgetc(file_ptr))) != EOF) {
        if (data->word[index] == '\n')
            line++;
        if (islower(data->word[index])) {
            if (pos == BEFORE_WORD) {
                pos = IN_WORD;
                data->line = line;
            }
            index++;
        }
        else if ((pos == IN_WORD) && (data->word[index] != '\'')) {
            break;
        }
    }
    /* Signals end of file has been reached. */
    if (data->word[index] == EOF)
        line = EOF;

    /* Adding the null character. */
    data->word[index] = '\0';

    return line;
}

/* Function inserts a word and it's line number into the hash table. */
void insert(char *inword, int in_line, Word_Node *table[])
{
    int position = hash(inword);
    Word_Node *curr, *prev = NULL;
    char dummy_word[DUMMY_WORD_LENGTH] = "A";

    /* The case where that hash position hasn't been used before; a new word
    ** node is created. 
    */
    if (table[position] == NULL)
        table[position] = new_word_node(dummy_word, 0);
    curr = table[position];

    /* Traverses that position's list of words until the current word is found
    ** (i.e. it's come up before) or the list end is reached (i.e. it's the
    ** first occurrence of the word).
    */
    while ((curr != NULL) && (strcmp(inword, curr->word) > 0)) {
        prev = curr;
        curr = curr->next_word;
    }

    /* If the word hasn't appeared before, it's inserted alphabetically into
    ** the list.
    */
    if ((curr == NULL) || (strcmp(curr->word, inword) != 0)) {
        prev->next_word = new_word_node(inword, in_line);
        prev->next_word->next_word = curr;
    }
    /* Otherwise, the word count is incremented, and the line number is added
    ** to the existing list.
    */
    else {
        (curr->number)++;
        curr->last_line = add_existing(curr->last_line, in_line);
    }
}

/* Function creates a new node for when a word is inserted for the first time.
*/
Word_Node *new_word_node(char *inword, int in_line)
{
    Word_Node *new;

    new = (Word_Node *) create(sizeof(Word_Node));
    new->word = (char *) create(sizeof(char) * (strlen(inword) + 1));
    new->word = strcpy(new->word, inword);
    /* The word count is set to 1, as this is the first occurrence! */
    new->number = 1;
    new->next_word = NULL;
    /* One line number node is added. */
    new->line_list = (Line_Node *) create(sizeof(Line_Node));
    new->line_list->line = in_line;
    new->line_list->next_line = NULL;
    new->last_line = new->line_list;

    return new;
}

/* Function adds a line number to the line number list of a word that has
** already been inserted at least once.  The pointer 'last_line', part of
** the word node structure, allows easy appending to the list.
*/
Line_Node *add_existing(Line_Node *last_line, int in_line)
{
    /* Check to see if that line has already occurred - multiple occurrences on
    ** the one line are only recorded once.  (Nb:  They are counted twice, but
    ** only listed once.)
    */
    if (last_line->line != in_line) {
        last_line->next_line = (Line_Node *) create(sizeof(Line_Node));
        last_line = last_line->next_line;
        last_line->line = in_line;
        last_line->next_line = NULL;
    }

    return last_line;
}

/*  Function controls the interactive command line part of the program. */
void interact(Word_Node *table[])
{
    char args[MAX_WORD_LENGTH];     /* Array to hold command line */
    Arg_Node *arg_list = NULL;      /* List that holds processed arguments */ 
    int not_quitted = TRUE;         /* Quit flag */

    /* The prompt (?) is displayed.  Commands are read into an array, and then
    ** individual arguments are placed into a linked list for easy use. 
    ** The first argument (actually the command) is looked at to determine
    ** what action should be performed.  'arg_list->next_arg' is passed to
    ** count() and list_lines(), because the actual 'c' or 'l' is not needed
    ** by them.  Lastly, the argument linked list is freed, by 'kill_arg_list'.  
    */ 
    do {
        printf("?");		     
        fgets(args, MAX_WORD_LENGTH - 1, stdin);
        arg_list = place_args_in_list(args);
        if (arg_list) {
            if (strcmp(arg_list->word, "c") == 0)
		count(arg_list->next_arg, table);
            else if (strcmp(arg_list->word, "l") == 0)
               	list_lines(arg_list->next_arg, table); 
            else if (strcmp(arg_list->word, "q") == 0) {
               	printf("Quitting concord\n");
		not_quitted = FALSE;
	    }
            else
               	printf("Not a valid command.\n");
	    kill_arg_list(arg_list);
        }
    } while (not_quitted);	/* Quits on flag */
}

/* Function takes an array containing a command line, and parses it, placing
** actual word into a linked list.
*/
Arg_Node *place_args_in_list(char command[])
{
    int index1 = 0, index2 = 0, pos = BEFORE_WORD;
    char token[MAX_WORD_LENGTH], c;
    Arg_Node *head = NULL;

    /* Non alphabetic characters are discarded.  Alphabetic characters are
    ** copied into the array 'token'.  Once the current word has been copied
    ** into 'token', 'append' is called, copying 'token' to a new node in the
    ** linked list.
    */
    while (command[index1] != '\0') {
        c = tolower(command[index1++]);
        if (islower(c)) {
            token[index2++] = c;
            pos = IN_WORD;
        }
        else if (c == '\'')
            token[index2] = c;
        else if (pos == IN_WORD) {
            pos = BEFORE_WORD;
            token[index2] = '\0';
            head = append(token, head);
            index2 = 0;
        }
    }

    return head;
}

/* Function takes a word, and appends a new node containing that word to the
** list.
*/
Arg_Node *append(char *word, Arg_Node *head)
{
    Arg_Node *curr = head,
             *new = (Arg_Node *) create(sizeof(Arg_Node));

    new->word = (char *) create(sizeof(char) * (strlen(word) + 1));
    strcpy(new->word, word);
    new->line_list = NULL;
    new->next_arg = NULL;

    if (head == NULL)
        return new;

    while (curr->next_arg != NULL)
        curr = curr->next_arg;
    curr->next_arg = new;

    return head;
}


/* Function displays the number of times a word has occurred. */
void count(Arg_Node *arg_list, Word_Node *table[])
{
    int hash_pos = 0;		/* Only initialised to avoid gnuc warnings */
    Word_Node *curr_word = NULL;  

    /* Checking for the right number of arguments (one). */
    if (arg_list) {
        if (arg_list->next_arg != NULL) {
	    printf("c requires only one argument\n");
	    return;
	}
        hash_pos = hash(arg_list->word);
    }
    else
	return;    

    /* Finds if the supplied word is in table, firstly by hashing to it's 
    ** would be position, and then traversing the list of words.  If present,
    ** it's number is displayed, otherwise '0' is printed. 
    */
    if (table[hash_pos]) {
	curr_word = table[hash_pos]->next_word;
	while ((curr_word != NULL) &&
	       (strcmp(arg_list->word, curr_word->word) != 0)) 
	    curr_word = curr_word->next_word; 	
        if (curr_word) 
  	    printf("%d\n", curr_word->number);
        else
	    printf("0\n");
    }
    else
	printf("0\n");
}

/* Function that takes each node in the argument list, and directs a pointer
** to that word's list of lines, which are present in the hash table. 
*/
void list_lines(Arg_Node *arg_head, Word_Node *table[])
{
    int hash_pos = 0;		/* Only initialised to avoid gnuc warnings */
    Word_Node *curr_word;
    Arg_Node *curr_arg = arg_head;

    /* For each word in the list of arguments, the word is looked for in the 
    ** hash table.  Each argument node has a pointer, and if the word is there,
    ** that pointer is set to point at that word's list of line numbers. 
    */ 
    while (curr_arg != NULL) {
        hash_pos = hash(curr_arg->word);
        if (table[hash_pos]) {
            curr_word = table[hash_pos]->next_word;   /* Gets past dummy node */
            while (curr_word != NULL && 
		   strcmp(curr_arg->word, curr_word->word) != 0) 
	        curr_word = curr_word->next_word;
            if (curr_word) 
	        curr_arg->line_list = curr_word->line_list;
        }
        curr_arg = curr_arg->next_arg;
    }
    /* An intersection is then performed, to determine which lines, if any, 
    ** all the arguments appear on.
    */
    if (arg_head)
        intersection(arg_head); 
}

/*  Function takes a list of line lists, and finds the lines that are common
**  to each line list, by using a comparison array.
*/
void intersection(Arg_Node *arg_head)
{
    Line_Node *curr_line;
    int *master, n = 0, index = 0, output = FALSE;
 
    /* Find size of first list, for creating master array */
    curr_line = arg_head->line_list;
    while (curr_line) {
        n++;
        curr_line = curr_line->next_line;
    }

    /* The master comparison array is created. */ 
    master = (int *) create(sizeof(int) * n);
    curr_line = arg_head->line_list;
 
    /*  Copy first list into master array */
    while (curr_line) {
        *(master + index++) = curr_line->line; 
	curr_line = curr_line->next_line;
    }

    /* Perform the actual intersection. */
    intersect_array(master, n, arg_head->next_arg);

    /* Print the line numbers left in the processed array, those left contain
    ** all the words specified in the command. 
    */
    for (index = 0; index < n; index++)
	if (*(master + index) != 0) { 
	    printf("%d ", *(master + index));
	    output = TRUE; 
	}
    /* 'Output' merely prevents an unnecessary newline when 'l' returns no 
    ** answer. 
    */
    if (output)
        printf("\n");

    /* Deallocate dynamic memory for master array */
    free(master);
}

/* Function takes master array containing line numbers - these depend on the
** first list of lines, and is done in 'list_lines'.  It then moves through the
** argument list.  For each word, each line number in master is compared to each
** line number in that word's line list.  If there is no match, then that 
** position in the array is set to 0, because that line is no longer in 
** contention as an answer.
*/
void intersect_array(int master[], int size, Arg_Node *arg_head)
{
    int index = 0;
    Line_Node *curr_line;

    while (arg_head) {
        index = 0;
        curr_line = arg_head->line_list;
    /* For each line in the list, any number less than that in the array will
    ** be set to zero.  Any number equal to that in the list will remain.
    ** This loop depends on the fact that both the line list, and the master 
    ** array, are sorted. */ 
        while (curr_line) {
            while (*(master + index) < curr_line->line && index < size)
                *(master + index++) = 0;
            while (*(master + index) <= curr_line->line && index < size)
                index++;
            curr_line = curr_line->next_line;
        }
    /* Once the list of lines has been traversed, any array positions that 
    ** haven't been examined are set to zero, as they are no longer in 
    ** contention. 
    */
        for ( ; index < size; index++)
            *(master + index) = 0;

        arg_head = arg_head->next_arg;
    }
}

/*  Function to free dynamic memory used by the arguments linked list. */
void kill_arg_list(Arg_Node *head)
{
    Arg_Node *temp;

    while (head != NULL) {
        temp = head;
        head = head->next_arg;
        free(temp->word);
        free(temp);
    }
}

