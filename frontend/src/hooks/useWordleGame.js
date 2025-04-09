import { useState, useRef, useEffect } from "react";
import {
  COLORS,
  MOCK_SUGGESTIONS,
  DEFAULT_SUGGESTIONS,
  ERROR_TIMEOUT,
} from "../utils/constants";
import { getNextColorUp, getNextColorDown } from "../utils/colorUtils";

/**
 * Custom hook that handles all Wordle game logic
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
  const [errorMessage, setErrorMessage] = useState("");

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

  // Focus the hidden input on initial render
  useEffect(() => {
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

  const generateSuggestions = () => {
    const constraints = grid.slice(0, currentRow + 1).map((row) =>
      row.map((cell) => ({
        letter: cell.letter.toLowerCase(),
        state: cell.color,
      })),
    );

    // TODO use API
    const newSuggestions =
      MOCK_SUGGESTIONS[currentRow - 1] || DEFAULT_SUGGESTIONS;
    setSuggestions(newSuggestions);
  };

  const handleKeyDown = (e) => {
    e.preventDefault();

    console.log("Key pressed:", e.key);
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
    } else if (e.key === "Enter") {
      const lettersComplete = grid[row].every((cell) => cell.letter !== "");
      const colorsComplete = grid[row].every(
        (cell) => cell.color !== COLORS.EMPTY,
      );

      if (lettersComplete && colorsComplete && row < 5) {
        setCurrentRow(row + 1);
        setCurrentCol(0);
        generateSuggestions();
      } else if (!lettersComplete) {
        setErrorMessage("All cells must have letters before proceeding");
        setTimeout(() => setErrorMessage(""), ERROR_TIMEOUT);
      } else if (!colorsComplete) {
        setErrorMessage("All cells must be colored before proceeding");
        setTimeout(() => setErrorMessage(""), ERROR_TIMEOUT);
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

  return {
    grid,
    currentRow,
    currentCol,
    suggestions,
    errorMessage,
    cellRefs,
    hiddenInputRef,
    handleKeyDown,
    handleCellClick,
  };
};

export default useWordleGame;
