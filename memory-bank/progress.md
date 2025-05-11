# LQPL Project Progress

## What Works
- Modular architecture with separate compiler, emulator, and frontend
- Command-line interface for compilation (lqpl binary)
- Server-based compiler (lqpl-compiler-server)
- Quantum program emulation (lqpl-emulator)
- GUI frontend for visual interaction (lqpl_gui.jar)
- Example LQPL programs in the lqplcode directory
- Comprehensive documentation in lqplManual.pdf

## What's Left to Build
- Additional documentation for developers and contributors
- Potential enhancements to the existing implementation
- Possible extensions to the language or tooling

## Current Status
- Release 0.9.0 is available
- The system is functional with the provided binaries and GUI
- The project is structured with separate backend and frontend components
- Documentation is available but could be expanded

## Known Issues
- None specifically documented in the README
- Issues can be reported to brett.giles@drogar.com or via GitHub

## Evolution of Project Decisions
- Modularization of the Compiler, Emulator, and Frontend
- Migration from Alex and Happy to Parsec for the Compiler
- Shift from Gtk2Hs to JRuby/Java/Swing for the frontend
- Separation of frontend into a distinct repository (http://github.com/drogar/lqpl-jruby-fe)
