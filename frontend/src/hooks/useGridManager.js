import { useState, useEffect, useRef } from "react";

// Cell color states
export const COLORS = {
  EMPTY: "empty",
  GRAY: "gray",
  YELLOW: "yellow",
  GREEN: "green",
};

// Create a simple initial grid for testing
const createInitialGrid = () => {
  return Array(6)
    .fill()
    .map(() => 
      Array(5).fill().map(() => ({ 
        letter: "", 
        color: COLORS.EMPTY 
      }))
    );
};

const useGridManager = () => {
  console.log("useGridManager hook is running");
  
  // State for the grid, current position, and suggestions
  const [grid, setGrid] = useState(createInitialGrid());
  const [currentRow, setCurrentRow] = useState(0);
  const [currentCol, setCurrentCol] = useState(0);
  const [suggestions, setSuggestions] = useState(["DUMMY", "TEST", "WORDS"]);

  // Refs for cell DOM elements - using a more direct approach
  const cellRefs = useRef([]);
  
  // Initialize cell refs only once - simplified version
  useEffect(() => {
    console.log("Initializing cell refs");
    cellRefs.current = Array(6).fill().map(() => Array(5).fill(null));
  }, []);

  // Simplified handler functions
  const handleKeyDown = (e, row, col) => {
    console.log("Key pressed:", e.key);
    // Implement a minimal version of key handling later
  };

  const handleCellClick = (row, col) => {
    console.log("Cell clicked:", row, col);
    setCurrentRow(row);
    setCurrentCol(col);
  };

  console.log("useGridManager returning values");
  
  return {
    grid,
    currentRow,
    currentCol,
    suggestions,
    cellRefs,
    handleKeyDown,
    handleCellClick,
  };
};

export default useGridManager;
