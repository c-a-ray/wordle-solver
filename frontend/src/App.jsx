import React, { useState, useRef, useEffect } from "react";
import Instructions from "./components/Instructions";
import Suggestions from "./components/Suggestions";

// Cell color states
const COLORS = {
  EMPTY: "empty",
  GRAY: "gray",
  YELLOW: "yellow",
  GREEN: "green",
};

function App() {
  // States for grid, current position, suggestions, and validation feedback
  const [grid, setGrid] = useState(
    Array(6).fill().map(() => Array(5).fill({ letter: "", color: COLORS.EMPTY }))
  );
  const [currentRow, setCurrentRow] = useState(0);
  const [currentCol, setCurrentCol] = useState(0);
  const [suggestions, setSuggestions] = useState(["WORDS", "WORLD", "WORTH", "WOULD", "WOUND"]);
  const [errorMessage, setErrorMessage] = useState("");
  
  // References for the cell elements and the main input
  const cellRefs = useRef(Array(6).fill().map(() => Array(5).fill(null)));
  const hiddenInputRef = useRef(null);
  
  // Focus the hidden input when position changes and on initial load
  useEffect(() => {
    // Focus the hidden input instead of the cell directly
    if (hiddenInputRef.current) {
      hiddenInputRef.current.focus();
    }
  }, [currentRow, currentCol]);
  
  // Make sure focus is applied on page load
  useEffect(() => {
    // Focus the input on initial page load
    if (hiddenInputRef.current) {
      // Force focus with a slight delay to ensure it works after component is fully rendered
      setTimeout(() => {
        hiddenInputRef.current.focus();
        console.log('Focus applied to input on page load');
      }, 100);
    }
  }, []);
  
  // Refocus the input whenever it might lose focus
  useEffect(() => {
    const handleWindowFocus = () => {
      if (hiddenInputRef.current) {
        hiddenInputRef.current.focus();
      }
    };
    
    // Add event listeners for focus and click
    window.addEventListener('focus', handleWindowFocus);
    document.addEventListener('click', handleWindowFocus);
    
    return () => {
      window.removeEventListener('focus', handleWindowFocus);
      document.removeEventListener('click', handleWindowFocus);
    };
  }, []);

  // Update a cell at the specified position
  const updateCell = (row, col, letter, color) => {
    const newGrid = [...grid];
    newGrid[row][col] = { letter, color };
    setGrid(newGrid);
  };

  // Move to the next cell
  const moveToNextCell = (row, col) => {
    if (col < 4) {
      setCurrentCol(col + 1);
    }
  };

  // Move to the previous cell
  const moveToPrevCell = (row, col) => {
    if (col > 0) {
      setCurrentCol(col - 1);
    }
  };

  // Cycle color up: empty -> gray -> yellow -> green -> empty
  const cycleColorUp = (row, col) => {
    const currentColor = grid[row][col].color;
    let newColor;

    switch (currentColor) {
      case COLORS.EMPTY:
        newColor = COLORS.GRAY;
        break;
      case COLORS.GRAY:
        newColor = COLORS.YELLOW;
        break;
      case COLORS.YELLOW:
        newColor = COLORS.GREEN;
        break;
      case COLORS.GREEN:
        newColor = COLORS.EMPTY;
        break;
      default:
        newColor = COLORS.EMPTY;
    }

    updateCell(row, col, grid[row][col].letter, newColor);
  };

  // Cycle color down: empty -> green -> yellow -> gray -> empty
  const cycleColorDown = (row, col) => {
    const currentColor = grid[row][col].color;
    let newColor;

    switch (currentColor) {
      case COLORS.EMPTY:
        newColor = COLORS.GREEN;
        break;
      case COLORS.GREEN:
        newColor = COLORS.YELLOW;
        break;
      case COLORS.YELLOW:
        newColor = COLORS.GRAY;
        break;
      case COLORS.GRAY:
        newColor = COLORS.EMPTY;
        break;
      default:
        newColor = COLORS.EMPTY;
    }

    updateCell(row, col, grid[row][col].letter, newColor);
  };

  // Generate new suggestions based on current constraints
  const generateSuggestions = () => {
    console.log("Generating suggestions based on constraints...");
    
    // Example constraints format for the API
    const constraints = grid.slice(0, currentRow + 1).map(row => 
      row.map(cell => ({
        letter: cell.letter.toLowerCase(),
        state: cell.color
      }))
    );
    
    console.log("Constraints:", constraints);
    
    // For now, simulate different suggestions based on the current row
    const mockSuggestions = [
      ["WORLD", "WORDS", "WORRY", "WORKS", "WORTH"],
      ["PRIZE", "PRIME", "PRINT", "PRIOR", "PRIDE"],
      ["TRACK", "TRAIN", "TRAIT", "TRAMP", "TRASH"],
      ["GHOST", "GLOOM", "GLORY", "GLOSS", "GLOVE"],
    ];
    
    // Use row index to get different suggestions
    const newSuggestions = mockSuggestions[currentRow - 1] || ["WORDS", "WORLD", "WORTH", "WOULD", "WOUND"];
    setSuggestions(newSuggestions);
  };

  // Unified keyboard input handling from hidden input
  const handleKeyDown = (e) => {
    e.preventDefault(); // Prevent all default actions
    
    console.log("Key pressed:", e.key);
    const row = currentRow;
    const col = currentCol;

    // Handle letter input (a-z)
    if (/^[a-zA-Z]$/.test(e.key)) {
      // Handle both uppercase and lowercase
      updateCell(row, col, e.key.toUpperCase(), grid[row][col].color);
      if (col < 4) {
        moveToNextCell(row, col);
      }
    }
    // Handle backspace
    else if (e.key === "Backspace" || e.key === "Delete") {
      if (grid[row][col].letter) {
        updateCell(row, col, "", grid[row][col].color);
      } else if (col > 0) {
        moveToPrevCell(row, col);
        updateCell(row, col - 1, "", grid[row][col - 1].color);
      }
    }
    // Handle arrow keys for navigation
    else if (e.key === "ArrowLeft") {
      moveToPrevCell(row, col);
    } else if (e.key === "ArrowRight") {
      moveToNextCell(row, col);
    }
    // Toggle cell color with up/down arrows
    else if (e.key === "ArrowUp") {
      cycleColorUp(row, col);
    } else if (e.key === "ArrowDown") {
      cycleColorDown(row, col);
    }
    // Move to next row when Enter is pressed and current row is complete (letters and colors)
    else if (e.key === "Enter") {
      // Check if all cells have letters
      const lettersComplete = grid[row].every((cell) => cell.letter !== "");
      
      // Check if all cells have colors (not EMPTY)
      const colorsComplete = grid[row].every((cell) => cell.color !== COLORS.EMPTY);
      
      // Only proceed if both checks pass
      if (lettersComplete && colorsComplete && row < 5) {
        setCurrentRow(row + 1);
        setCurrentCol(0);
        generateSuggestions();
      } else if (!lettersComplete) {
        setErrorMessage("All cells must have letters before proceeding");
        setTimeout(() => setErrorMessage(""), 2000); // Clear message after 2 seconds
      } else if (!colorsComplete) {
        setErrorMessage("All cells must be colored before proceeding");
        setTimeout(() => setErrorMessage(""), 2000); // Clear message after 2 seconds
      }
    }
  };
  
  // When user clicks on a cell, update current position if valid and focus hidden input
  const handleCellClick = (row, col) => {
    // Check if the clicked row is accessible
    // Only allow clicking on cells in current row or in completed rows
    if (row <= currentRow) {
      setCurrentRow(row);
      setCurrentCol(col);
      if (hiddenInputRef.current) {
        hiddenInputRef.current.focus();
      }
    }
  };

  // Get background color based on cell state and row status
  const getCellStyle = (color, isActive, rowIndex) => {
    // Determine if this cell is in a row that is still locked
    const isDisabled = rowIndex > currentRow;

    const baseStyle = {
      width: "3.5rem",
      height: "3.5rem",
      border: "2px solid #ccc",
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      fontSize: "1.5rem",
      fontWeight: "bold",
      cursor: isDisabled ? "not-allowed" : "pointer",
      transition: "all 0.2s",
      userSelect: "none", // Prevent text selection
      outline: isActive ? "3px solid #888" : "none",
      opacity: isDisabled ? 0.5 : 1,
    };

    switch (color) {
      case COLORS.GRAY:
        return { 
          ...baseStyle, 
          backgroundColor: "#6e6e6e", 
          borderColor: "#6e6e6e",
          color: "white"
        };
      case COLORS.YELLOW:
        return { 
          ...baseStyle, 
          backgroundColor: "#c9b458", 
          borderColor: "#c9b458",
          color: "white"
        };
      case COLORS.GREEN:
        return { 
          ...baseStyle, 
          backgroundColor: "#6aaa64", 
          borderColor: "#6aaa64",
          color: "white"
        };
      default:
        return {
          ...baseStyle,
          backgroundColor: isActive ? "#f0f0f0" : "#fff",
          borderColor: isActive ? "#888" : "#ccc"
        };
    }
  };

  return (
    <div className="min-h-screen bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 p-4 flex flex-col items-center justify-center">
      <div className="w-full max-w-2xl mx-auto flex flex-col items-center">
        <h1 className="text-4xl font-bold p-4 mb-2 text-center">Wordle Solver</h1>
        
        {/* Error message display */}
        {errorMessage && (
          <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-2 rounded mb-4 text-center w-full">
            {errorMessage}
          </div>
        )}
        
        {/* Hidden input to capture keyboard events */}
        <input
          ref={hiddenInputRef}
          type="text"
          autoFocus
          autoComplete="off"
          onClick={(e) => e.preventDefault()}
          onFocus={(e) => e.currentTarget.focus()}
          style={{
            position: "fixed",
            top: 0,
            left: 0,
            opacity: 0,
            pointerEvents: "none",
            width: "1px",
            height: "1px",
            zIndex: -1,
          }}
          onKeyDown={handleKeyDown}
        />
        
        {/* Interactive Grid with improved focus handling */}
        <div style={{margin: "0 auto 1.5rem", display: "flex", justifyContent: "center", width: "100%"}}>
          <div style={{display: "flex", flexDirection: "column", gap: "0.5rem"}}>
            {grid.map((row, rowIndex) => (
              <div key={`row-${rowIndex}`} style={{display: "flex", gap: "0.5rem", justifyContent: "center"}}>
                {row.map((cell, colIndex) => {
                  const isActive = rowIndex === currentRow && colIndex === currentCol;
                  return (
                    <div
                      key={`cell-${rowIndex}-${colIndex}`}
                      ref={el => cellRefs.current[rowIndex][colIndex] = el}
                      style={getCellStyle(cell.color, isActive, rowIndex)}
                      onClick={() => handleCellClick(rowIndex, colIndex)}
                      tabIndex={-1}
                    >
                      {cell.letter}
                    </div>
                  );
                })}
              </div>
            ))}
          </div>
        </div>

        <div className="mb-4 w-full text-center">
          <p className="text-center">Click a cell and type to enter a word. Use ↑/↓ to change colors.</p>
          <p className="text-center">Press Enter when your word is complete to get suggestions.</p>
        </div>

        <div className="w-full flex flex-col items-center">
          <Instructions />
          <Suggestions suggestions={suggestions} />
        </div>
      </div>
    </div>
  );
}

export default App;