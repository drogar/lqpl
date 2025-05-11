# LQPL Technical Context

## Technologies Used
- **Backend:**
  - Haskell (compiler and emulator implementation)
  - Parsec (for parsing, replacing previous Alex and Happy dependencies)
  - Alex and Happy (still used for the assembler in the emulator)

- **Frontend:**
  - Java/JRuby (for the GUI implementation)
  - Swing (for GUI components)

## Development Setup
- Java 8 or higher runtime environment required
- Backend binaries: lqpl, lqpl-compiler-server, lqpl-emulator
- Frontend: lqpl_gui.jar

## Technical Constraints
- Requires Java 8+ runtime environment
- Modular architecture requiring communication between components
- Separate repositories for backend and frontend components

## Dependencies
- Java Runtime Environment (JRE) 8+
- Various redistributed JARs for the frontend (contained in lib directory)

## Tool Usage Patterns
- **Command Line Usage:**
  - Direct use of the `lqpl` binary for compilation
  - Integration with the emulator for program execution

- **GUI Usage:**
  - Launch via `java -jar lqpl_gui.jar`
  - Provides interface to both compiler and emulator
  - Visual representation of program execution
