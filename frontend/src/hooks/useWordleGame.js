import { useState, useRef, useEffect } from "react";
import { COLORS, ERROR_TIMEOUT } from "../utils/constants";
import { getNextColorUp, getNextColorDown } from "../utils/colorUtils";
import {
  submitGuess,
  resetGame,
  convertColorsForBackend,
  DEFAULT_SUGGESTIONS,
} from "../services/api";

/**
 * Custom hook that handles all Wordle game logic
 * @returns {Object} Game state and handler functions
 */
const useWordleGame = () => {
  // Game state
  const [grid, setGrid] = useState(
    Array(6)
      .fill()
      .map(() => Array(5).fill({ letter: "", color: COLORS.EMPTY })),
  );
  const [currentRow, setCurrentRow] = useState(0);
  const [currentCol, setCurrentCol] = useState(0);
  const [suggestions, setSuggestions] = useState(DEFAULT_SUGGESTIONS);
  const [errorMessage, setErrorMessage] = useState("");
  const [isLoading, setIsLoading] = useState(false);

  // Refs
  const cellRefs = useRef(
    Array(6)
      .fill()
      .map(() => Array(5).fill(null)),
  );
  const hiddenInputRef = useRef(null);

  // Focus the hidden input when current cell changes
  useEffect(() => {
    if (hiddenInputRef.current) {
      hiddenInputRef.current.focus();
    }
  }, [currentRow, currentCol]);

  // Initialize the game
  useEffect(() => {
    // Just initialize with default suggestions - don't call backend
    setSuggestions(DEFAULT_SUGGESTIONS);

    // Focus the hidden input on initial render
    if (hiddenInputRef.current) {
      setTimeout(() => {
        hiddenInputRef.current.focus();
      }, 100);
    }
  }, []);

  // Grid manipulation functions
  const updateCell = (row, col, letter, color) => {
    const newGrid = [...grid];
    newGrid[row][col] = { letter, color };
    setGrid(newGrid);
  };

  const moveToNextCell = (row, col) => {
    if (col < 4) {
      setCurrentCol(col + 1);
    }
  };

  const moveToPrevCell = (row, col) => {
    if (col > 0) {
      setCurrentCol(col - 1);
    }
  };

  const cycleColorUp = (row, col) => {
    const currentColor = grid[row][col].color;
    const newColor = getNextColorUp(currentColor);
    updateCell(row, col, grid[row][col].letter, newColor);
  };

  const cycleColorDown = (row, col) => {
    const currentColor = grid[row][col].color;
    const newColor = getNextColorDown(currentColor);
    updateCell(row, col, grid[row][col].letter, newColor);
  };

  // Submit guess to backend and get suggestions
  const generateSuggestions = async () => {
    try {
      setIsLoading(true);
      const rowData = grid[currentRow];

      // Extract the word from the current row
      const word = rowData
        .map((cell) => cell.letter)
        .join("")
        .toLowerCase();

      // Convert colors to the format expected by the backend
      const colors = convertColorsForBackend(grid, currentRow);

      console.log(
        `Submitting guess: word=${word}, colors=${colors}, guessNumber=${currentRow + 1}`,
      );

      // Submit the guess to the server - the only time we communicate with backend
      const newSuggestions = await submitGuess(word, colors, currentRow + 1);

      // Update the UI with the new suggestions
      setSuggestions(newSuggestions);

      if (newSuggestions === null) {
        console.log("No words match the pattern");
      } else {
        console.log("Received new suggestions:", newSuggestions);
      }

      return true;
    } catch (error) {
      console.error("Error generating suggestions:", error);
      setErrorMessage(`Failed to get suggestions: ${error.message}`);
      setTimeout(() => setErrorMessage(""), ERROR_TIMEOUT);
      return false;
    } finally {
      setIsLoading(false);
    }
  };

  // Handle keyboard input
  const handleKeyDown = (e) => {
    e.preventDefault();

    if (isLoading) return; // Prevent input during API calls

    console.log("Key pressed:", e.key);
    const row = currentRow;
    const col = currentCol;

    if (/^[a-zA-Z]$/.test(e.key)) {
      // Letter key pressed
      updateCell(row, col, e.key.toUpperCase(), grid[row][col].color);
      if (col < 4) {
        moveToNextCell(row, col);
      }
    } else if (e.key === "Backspace" || e.key === "Delete") {
      // Backspace/Delete key pressed
      if (grid[row][col].letter) {
        updateCell(row, col, "", grid[row][col].color);
      } else if (col > 0) {
        moveToPrevCell(row, col);
        updateCell(row, col - 1, "", grid[row][col - 1].color);
      }
    } else if (e.key === "ArrowLeft") {
      // Left arrow key pressed
      moveToPrevCell(row, col);
    } else if (e.key === "ArrowRight") {
      // Right arrow key pressed
      moveToNextCell(row, col);
    } else if (e.key === "ArrowUp") {
      // Up arrow key pressed
      cycleColorUp(row, col);
    } else if (e.key === "ArrowDown") {
      // Down arrow key pressed
      cycleColorDown(row, col);
    } else if (e.key === "ArrowLeft") {
      // Left arrow key pressed
      moveToPrevCell(row, col);
    } else if (e.key === "ArrowRight") {
      // Right arrow key pressed
      moveToNextCell(row, col);
    } else if (e.key === "Home") {
      // Home key pressed - move to first cell in row
      setCurrentCol(0);
    } else if (e.key === "End") {
      // End key pressed - move to last cell in row
      setCurrentCol(4);
    } else if (e.key === "Enter") {
      // Enter key pressed
      const lettersComplete = grid[row].every((cell) => cell.letter !== "");
      const colorsComplete = grid[row].every(
        (cell) => cell.color !== COLORS.EMPTY,
      );

      if (lettersComplete && colorsComplete && row < 5) {
        // If complete, move to next row and generate suggestions
        generateSuggestions()
          .then((success) => {
            if (success) {
              setCurrentRow(row + 1);
              setCurrentCol(0);
            }
          })
          .catch((error) => {
            console.error("Error handling Enter key:", error);
          });
      } else if (!lettersComplete) {
        // If letters incomplete, show error
        setErrorMessage("All cells must have letters before proceeding");
        setTimeout(() => setErrorMessage(""), ERROR_TIMEOUT);
      } else if (!colorsComplete) {
        // If colors incomplete, show error
        setErrorMessage("All cells must be colored before proceeding");
        setTimeout(() => setErrorMessage(""), ERROR_TIMEOUT);
      }
    }
  };

  // Handle clicking on a cell
  const handleCellClick = (row, col) => {
    if (row <= currentRow) {
      setCurrentRow(row);
      setCurrentCol(col);
      if (hiddenInputRef.current) {
        hiddenInputRef.current.focus();
      }
    }
  };

  // Expose a reset function
  const resetGameState = async () => {
    try {
      setIsLoading(true);

      // Reset the server state
      try {
        await resetGame();
        console.log("Game reset successfully on server");
      } catch (error) {
        console.warn("Could not reset game on server:", error);
      }

      // Reset the local state
      setGrid(
        Array(6)
          .fill()
          .map(() => Array(5).fill({ letter: "", color: COLORS.EMPTY })),
      );
      setCurrentRow(0);
      setCurrentCol(0);
      setSuggestions(DEFAULT_SUGGESTIONS);

      setErrorMessage("Game reset successfully");
      setTimeout(() => setErrorMessage(""), ERROR_TIMEOUT);
    } catch (error) {
      console.error("Error resetting game:", error);
      setErrorMessage(`Failed to reset game: ${error.message}`);
      setTimeout(() => setErrorMessage(""), ERROR_TIMEOUT);
    } finally {
      setIsLoading(false);
    }
  };

  return {
    grid,
    currentRow,
    currentCol,
    suggestions,
    errorMessage,
    isLoading,
    cellRefs,
    hiddenInputRef,
    handleKeyDown,
    handleCellClick,
    resetGameState,
  };
};

export default useWordleGame;
