# fiblib
Fibonacci heap in ocaml

    Fibonacci heaps are kind of an obscure data structure, but they have their
    purposes. Operations that do not delete an element run in [O(1)] amortised
    time. In particular, if the number of [extract-min] and [delete] operations is
    small compared to the number of other operations, fibonacci heaps are a win
    (CLRS 477).

    In this library, each heap is conceptually a min-heap (elements are closer
    to the root whose keys are less than other keys). However, the comparison
    function is parameterised by functorisation, so it is just as easy to create a
    min-heap as a max-heap.

    In particular, if your keys are integers, and the comparison function is
    {!Pervasives.compare}, you will get a min-heap, e.g., data with key 5 will come
    out of the heap before data with key 10.
