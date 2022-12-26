package main

import (
	"fmt"
	"time"
)

type TaskHandler func(task Task) error

// AddTask adds a task with the given priority to the heap. Priority 1 is
// the highest signficance and are processed first, then priority 2, etc.
func AddTask(heap *Heap[Task], priority int, handler TaskHandler) Task {
	var task Task = Task{
		Created:  time.Now(),
		Priority: priority,
		Handler:  handler,
	}

	heap.Add(task)

	return task
}

type Task struct {
	Created  time.Time
	Priority int
	Handler  TaskHandler
}

func (task Task) String() string {
	return fmt.Sprintf("Task created at %s", task.Created)
}

func (task Task) LessThan(other Task) bool {
	return task.Priority < other.Priority
}

// Binary Min-Heap //

type HeapItem[T any] interface {
	fmt.Stringer
	LessThan(other T) bool
}

type Heap[T HeapItem[T]] struct {
	items []T
}

func (h *Heap[T]) Add(item T) {
	h.items = append(h.items, item)
	h.heapifyUp(len(h.items) - 1)
}

func (h *Heap[T]) heapifyUp(i int) {
	for i > 0 {
		parent := parentIndex(i)

		if !h.items[i].LessThan(h.items[parent]) {
			// Min-heap condition restored; stop here
			break
		}

		h.swap(parent, i)
		i = parent
	}
}

func (h *Heap[T]) heapifyDown(i int) {
	for {
		left, right := childIndexes(i)
		min := i

		if left < len(h.items) && h.items[left].LessThan(h.items[min]) {
			min = left
		}

		if right < len(h.items) && h.items[right].LessThan(h.items[min]) {
			min = right
		}

		if min == i {
			break
		}

		h.swap(i, min)
		i = min
	}
}

func (h *Heap[T]) PopMin() (T, bool) {
	if len(h.items) == 0 {
		return *new(T), false
	}

	item := h.items[0]
	last := len(h.items) - 1

	h.swap(0, last)

	// Remove the old cell from the end of the heap and indexes
	h.items = h.items[:last]

	if len(h.items) > 0 {
		// Move the swapped session to where it should be in the heap
		h.heapifyDown(0)
	}

	return item, true
}

func (h *Heap[T]) swap(i, j int) {
	h.items[i], h.items[j] = h.items[j], h.items[i]
}

func parentIndex(i int) int {
	return (i - 1) / 2
}

func childIndexes(i int) (left, right int) {
	return 2*(i+1) - 1, 2 * (i + 1)
}
