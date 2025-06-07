# dsa/toolchain

This is to be a collection of software for programming for the [DSA architecture](https://github.com/nullndvoid/custom_isa_design).
The primary goals are to write _a VM_ (currently an emulator, but we may look into extending QEMU) and _an assembler_.

This repo will replace the Rust programs in the [main repository](https://github.com/nullndvoid/custom_isa_design), as that will move to just being used for documentation.

# Roadmap

- Refactor the emulator frontend, backend, and assembler into separate programs.
- Possibly extend [QEMU](https://www.qemu.org/), this might be better in the long run, despite the complexity of QEMU.

# Authors

- [@zxq5](https://github.com/zxq5-dev), who wrote a good chunk of the prototype code for this project, including the egui emulator and assembler.
- [@nullndvoid](https://github.com/nullndvoid)
