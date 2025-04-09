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
 * Handles all Wordle game logic
 * @returns {Object} Game state and handler functions
 */
const useWordleGame = () => {
  const [grid, setGrid] = useState(
    Array(6)
      .fill()
      .map(() => Array(5).fill({ letter: "", color: COLORS.EMPTY })),
  );
  const [currentRow, setCurrentRow] = useState(0);
  const [currentCol, setCurrentCol] = useState(0);
  const [suggestions, setSuggestions] = useState(DEFAULT_SUGGESTIONS);
  const [message, setMessage] = useState("");
  const [isLoading, setIsLoading] = useState(false);

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
    const initGame = async () => {
      setSuggestions(DEFAULT_SUGGESTIONS);

      try {
        setIsLoading(true);
        await resetGame();
      } finally {
        setIsLoading(false);
      }
    };

    initGame();

    // Focus the hidden input on initial render
    if (hiddenInputRef.current) {
      setTimeout(() => {
        hiddenInputRef.current.focus();
      }, 100);
    }
  }, []);

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

  const generateSuggestions = async () => {
    try {
      setIsLoading(true);
      const rowData = grid[currentRow];
      const word = rowData
        .map((cell) => cell.letter)
        .join("")
        .toLowerCase();
      const colors = convertColorsForBackend(grid, currentRow);
      const newSuggestions = await submitGuess(word, colors, currentRow + 1);
      setSuggestions(newSuggestions);
      return true;
    } catch (error) {
      setMessage(`Failed to get suggestions: ${error.message}`);
      setTimeout(() => setMessage(""), ERROR_TIMEOUT);
      return false;
    } finally {
      setIsLoading(false);
    }
  };

  const handleKeyDown = (e) => {
    e.preventDefault();

    if (isLoading) return;

    const row = currentRow;
    const col = currentCol;

    if (/^[a-zA-Z]$/.test(e.key)) {
      updateCell(row, col, e.key.toUpperCase(), grid[row][col].color);
      if (col < 4) {
        moveToNextCell(row, col);
      }
    } else if (e.key === "Backspace" || e.key === "Delete") {
      if (grid[row][col].letter) {
        updateCell(row, col, "", grid[row][col].color);
      } else if (col > 0) {
        moveToPrevCell(row, col);
        updateCell(row, col - 1, "", grid[row][col - 1].color);
      }
    } else if (e.key === "ArrowLeft") {
      moveToPrevCell(row, col);
    } else if (e.key === "ArrowRight") {
      moveToNextCell(row, col);
    } else if (e.key === "ArrowUp") {
      cycleColorUp(row, col);
    } else if (e.key === "ArrowDown") {
      cycleColorDown(row, col);
    } else if (e.key === "ArrowLeft") {
      moveToPrevCell(row, col);
    } else if (e.key === "ArrowRight") {
      moveToNextCell(row, col);
    } else if (e.key === "Home") {
      setCurrentCol(0);
    } else if (e.key === "End") {
      setCurrentCol(4);
    } else if (e.key === "Enter") {
      const lettersComplete = grid[row].every((cell) => cell.letter !== "");
      const colorsComplete = grid[row].every(
        (cell) => cell.color !== COLORS.EMPTY,
      );

      if (lettersComplete && colorsComplete && row < 5) {
        generateSuggestions().then((success) => {
          if (success) {
            setCurrentRow(row + 1);
            setCurrentCol(0);
          }
        });
      } else if (!lettersComplete) {
        setMessage("All cells must have letters before proceeding");
        setTimeout(() => setMessage(""), ERROR_TIMEOUT);
      } else if (!colorsComplete) {
        setMessage("All cells must be colored before proceeding");
        setTimeout(() => setMessage(""), ERROR_TIMEOUT);
      }
    }
  };

  const handleCellClick = (row, col) => {
    if (row <= currentRow) {
      setCurrentRow(row);
      setCurrentCol(col);
      if (hiddenInputRef.current) {
        hiddenInputRef.current.focus();
      }
    }
  };

  const resetGameState = async () => {
    try {
      setIsLoading(true);
      await resetGame();

      setGrid(
        Array(6)
          .fill()
          .map(() => Array(5).fill({ letter: "", color: COLORS.EMPTY })),
      );
      setCurrentRow(0);
      setCurrentCol(0);
      setSuggestions(DEFAULT_SUGGESTIONS);
      setMessage("Game reset successfully");
      setTimeout(() => setMessage(""), ERROR_TIMEOUT);
    } catch (error) {
      setMessage(`Failed to reset game: ${error.message}`);
      setTimeout(() => setMessage(""), ERROR_TIMEOUT);
    } finally {
      setIsLoading(false);
    }
  };

  return {
    grid,
    currentRow,
    currentCol,
    suggestions,
    message,
    isLoading,
    cellRefs,
    hiddenInputRef,
    handleKeyDown,
    handleCellClick,
    resetGameState,
  };
};

export default useWordleGame;
