//! this will be a multithreaded implementation of the Mark And Sweep GC
//!
//! Multithreaded in this case means, that the Marking Phase runs on a seperate thread that
//! periodically marks memory to be freed, while the sweeping phase runs on the main thread and
//! "stops the world"
//!
//! it will implement the Allocator interface to be easily swappable with other GC implementations
