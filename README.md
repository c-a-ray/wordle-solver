# Wordle Solver

A tool to help solve Wordle puzzles by suggesting optimal word choices based on your previous guesses and feedback.

## Overview

This project is a full-stack application with:

- A Common Lisp backend that provides word suggestions based on the current game state
- A React frontend with an interactive Wordle-like UI for entering guesses and receiving suggestions

## Prerequisites

- [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
- [Node.js](https://nodejs.org/)
- [npm](https://www.npmjs.com/)

## Getting Started

### Backend Setup

1. Start SBCL in your terminal:
   ```bash
   sbcl
   ```

2. Load the system with Quicklisp:
   ```lisp
   (ql:quickload :wordle-solver)
   ```

3. Start the server (default port is 8080):
   ```lisp
   (solver-server:start-server)
   ```

### Frontend Setup

1. Navigate to the frontend directory:
   ```bash
   cd frontend
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Start the development server:
   ```bash
   npm run dev
   ```

4. Access the application at `http://localhost:3000`

## Usage

1. Enter your Wordle guess in the current row
2. Use the arrow keys to navigate between cells
3. Use the up/down arrow keys to cycle through colors (green, yellow, gray)
4. Press Enter to submit your guess
5. Review the suggestions in the sidebar
6. Click on a suggestion to fill the current row with that word
7. Use the Reset button to start a new game

## Data Sources

- The full word list was taken from [tabatkins/wordle-list](https://github.com/tabatkins/wordle-list), which is licensed under an [MIT License](https://github.com/tabatkins/wordle-list/blob/main/LICENSE).
- Word frequency data is derived from [Peter Norvig's compilation](https://norvig.com/google-books-common-words.txt), based on the [Google Books Ngram data](https://books.google.com/ngrams/info), which is licensed under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/).

## License

MIT

## Author

Cody Ray
