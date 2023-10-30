The `CTask` backend basically targets software implementations using a real-time operating system
(RTOS). 

Each model instance is translated into a "task". The behavior of the task in described in a C-like
pseudo-code. The type `event` corresponds to (external or internal) events. The type `int<n>`
corresponds to `n`-bit integers. The type `int<l:h>` corresponds to integers ranging from `l` to
`h`.  The primitive `wait_ev(e)` blocks the calling task until event `e` is received. 

The implementation of these types / primitives will depend on the actual target RTOS.
