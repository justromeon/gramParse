# gramParse

Welcome to gramParse, a Haskell project demonstrating parsing and converting a custom language into JSON using Cabal.

## Project Structure

- `app/`: Contains the main application code.
  - `Main.hs`: Entry point of the application.
- `src/`: Contains supporting modules.
  - `Parser.hs`: Defines parsers for a custom language and converts parsed data into JSON.
- `input/`: Directory where input files are stored.
  - `sample.txt`: Sample input file for testing purposes.
- `output/`: Directory where output JSON files are generated.

## Prerequisites

To build and run this project, you need:
- GHC (Glasgow Haskell Compiler)
- Cabal (Haskell build tool)

Ensure these are installed and configured on your system.

## Building the Project

1. Clone this repository to your local machine.
2. Navigate to the project directory.
3. Run the following commands:

   ```bash
   cabal update
   cabal build

## Running the Application with `sample.txt`

   ```bash
   cabal run
   Enter the name of the input text file (inside 'input' directory): sample.txt
   Enter the name for the output JSON file (inside 'output' directory): sample.json
   Successfully parsed and wrote JSON to: output/sample.json
