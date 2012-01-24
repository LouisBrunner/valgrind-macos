/*
  This file is part of drd, a thread error detector.

  Copyright (C) 1990-2011 Linus Torvalds and other kernel authors.
  Copyright (C) 2012 Bart Van Assche <bvanassche@acm.org>.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  02111-1307, USA.

  The GNU General Public License is contained in the file COPYING.
*/

#ifndef _DRD_LIST_H_
#define _DRD_LIST_H_

/*
 * Doubly linked lists. See also the Linux kernel headers <linux/types.h>.
 */

/**
 * container_of - cast a member of a structure out to the containing structure
 * @ptr:        the pointer to the member.
 * @type:       the type of the container struct this is embedded in.
 * @member:     the name of the member within the struct.
 *
 */
#define container_of(ptr, type, member) ({                      \
         const typeof( ((type *)0)->member ) *__mptr = (ptr);   \
         (type *)( (char *)__mptr - offsetof(type,member) );})

struct list_head {
   struct list_head *next;
   struct list_head *prev;
};

#define LIST_HEAD_INIT(name) { &(name), &(name) }

static inline void init_list_head(struct list_head *list)
{
   list->next = list;
   list->prev = list;
}

static inline void __list_add(struct list_head *new,
                              struct list_head *prev,
                              struct list_head *next)
{
   next->prev = new;
   new->next = next;
   new->prev = prev;
   prev->next = new;
}

/**
 * list_add - add a new entry
 * @new: new entry to be added
 * @head: list head to add it after
 *
 * Insert a new entry after the specified head.
 */
static inline void list_add(struct list_head *new, struct list_head *head)
{
   __list_add(new, head, head->next);
}

/**
 * list_add_tail - add a new entry
 * @new: new entry to be added
 * @head: list head to add it before
 *
 * Insert a new entry before the specified head.
 * This is useful for implementing queues.
 */
static inline void list_add_tail(struct list_head *new, struct list_head *head)
{
   __list_add(new, head->prev, head);
}

/*
 * Delete a list entry by making the prev/next entries
 * point to each other.
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static inline void __list_del(struct list_head * prev, struct list_head * next)
{
   next->prev = prev;
   prev->next = next;
}

/**
 * list_del - deletes entry from list.
 * @entry: the element to delete from the list.
 * Note: list_empty() on entry does not return true after this, the entry is
 * in an undefined state.
 */
static inline void list_del(struct list_head *entry)
{
   __list_del(entry->prev, entry->next);
   entry->next = NULL;
   entry->prev = NULL;
}

/**
 * list_is_last - tests whether @list is the last entry in list @head
 * @list: the entry to test
 * @head: the head of the list
 */
static inline int list_is_last(const struct list_head *list,
                                const struct list_head *head)
{
   return list->next == head;
}

/**
 * list_empty - tests whether a list is empty
 * @head: the list to test.
 */
static inline int list_empty(const struct list_head *head)
{
   return head->next == head;
}

/**
 * list_entry - get the struct for this entry
 * @ptr:        the &struct list_head pointer.
 * @type:       the type of the struct this is embedded in.
 * @member:     the name of the list_struct within the struct.
 */
#define list_entry(ptr, type, member) \
   container_of(ptr, type, member)

/**
 * list_first_entry - get the first element from a list
 * @ptr:        the list head to take the element from.
 * @type:       the type of the struct this is embedded in.
 * @member:     the name of the list_struct within the struct.
 *
 * Note, that list is expected to be not empty.
 */
#define list_first_entry(ptr, type, member) \
   list_entry((ptr)->next, type, member)

#define list_next_entry(ptr, type, member) \
   list_entry((ptr)->next, type, member)

#define list_last_entry(ptr, type, member) \
   list_entry((ptr)->prev, type, member)

/**
 * list_for_each_entry  -       iterate over list of given type
 * @pos:        the type * to use as a loop cursor.
 * @head:       the head for your list.
 * @member:     the name of the list_struct within the struct.
 */
#define list_for_each_entry(pos, head, member)                          \
   for (pos = list_entry((head)->next, typeof(*pos), member);           \
        &pos->member != (head);                                         \
        pos = list_entry(pos->member.next, typeof(*pos), member))

/**
 * list_for_each_entry_reverse - iterate backwards over list of given type.
 * @pos:        the type * to use as a loop cursor.
 * @head:       the head for your list.
 * @member:     the name of the list_struct within the struct.
 */
#define list_for_each_entry_reverse(pos, head, member)                  \
   for (pos = list_entry((head)->prev, typeof(*pos), member);           \
        &pos->member != (head);                                         \
        pos = list_entry(pos->member.prev, typeof(*pos), member))

#define list_for_each_entry_reverse_continue(pos, head, member)         \
   for ( ;                                                              \
        &pos->member != (head);                                         \
        pos = list_entry(pos->member.prev, typeof(*pos), member))

/**
 * list_for_each_entry_safe - iterate over list of given type safe against removal of list entry
 * @pos:        the type * to use as a loop cursor.
 * @n:          another type * to use as temporary storage
 * @head:       the head for your list.
 * @member:     the name of the list_struct within the struct.
 */
#define list_for_each_entry_safe(pos, n, head, member)                  \
   for (pos = list_entry((head)->next, typeof(*pos), member),           \
           n = list_entry(pos->member.next, typeof(*pos), member);      \
        &pos->member != (head);                                         \
        pos = n, n = list_entry(n->member.next, typeof(*n), member))

#endif /* _DRD_LIST_H_ */
