An emulator of a novel computer architecture modeled after the [Fresh Breeze][]
design, intended for exploring programming such a computer with its unusual
chunk memory management and multi-core synchronization, that enables amazing
degrees of modularity, and even compositionality, of libraries and programs that
are very parallel.  Vifne means fresh in the Lojban language.

The purpose is only to achieve the simplest implementation of an ISA
(instruction set architecture) and truly-parallel multiprocessing that captures
the essential nature of the Fresh Breeze design.  It does not emulate many
aspects of a real hardware machine, and it differs from the Fresh Breeze design,
as described in the freely-available papers, in some ways.

Unfortunately, I wasn't able to find the time to complete it after encountering
a problem with reference cycles and streams, but the emulator and assembler are
fairly built-out and do work.  I did record notes about the problem, and the
Fresh Breeze papers do mention a solution for avoiding the problem which would
be cool to research and incorporate.

Running the emulator and the tests requires [my modified version of Ikarus
Scheme][Ikarus-mod] for the exit-handler facility.  Contact me if you're
interested and unsure how to build that.  It'd probably be straightforward to
add support for other R6RS systems that provide an exit handler and an FFI.

[Fresh Breeze]: http://csg.csail.mit.edu/Users/dennis/
[Ikarus-mod]: http://code.launchpad.net/~derick-eddington/ikarus/ikarus.dev-derick
