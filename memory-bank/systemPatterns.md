# LQPL System Patterns

## System Architecture
LQPL follows a modular architecture with three main components:
1. **Compiler** - Translates LQPL source code into executable form
2. **Emulator** - Executes the compiled quantum programs
3. **GUI Frontend** - Provides visual interface for interaction with the system

## Key Technical Decisions
- **Modularization** - Separation of compiler, emulator, and frontend for better maintainability
- **Language Choice** - Haskell for backend implementation, aligning with the functional paradigm of the language itself
- **Frontend Technology** - Shift from Gtk2Hs to JRuby/Java/Swing for better cross-platform compatibility
- **Parser Implementation** - Migration from Alex/Happy to Parsec for the compiler (though assembler still uses Alex/Happy)

## Design Patterns
- **Client-Server Model** - The compiler server (lqpl-compiler-server) provides services to the frontend
- **Functional Programming Paradigm** - Consistent with the language's own paradigm
- **Model-View-Controller** - Likely used in the GUI implementation to separate concerns

## Component Relationships
```
+----------------+      +----------------+
|                |      |                |
|  GUI Frontend  |<---->|   Compiler    |
| (JRuby/Java)   |      |   (Haskell)   |
|                |      |                |
+----------------+      +----------------+
        |                      |
        |                      |
        v                      v
+----------------+      +----------------+
|                |      |                |
|  User Input/   |      |   Emulator    |
|  Visualization |<---->|   (Haskell)   |
|                |      |                |
+----------------+      +----------------+
```

## Critical Implementation Paths
1. Source code parsing and compilation
2. Quantum state representation and manipulation
3. Communication between frontend and backend components
4. Visualization of quantum program execution
