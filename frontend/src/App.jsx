import React, { useState, useRef, useEffect } from "react";
import Sidebar from "./components/Sidebar";

const COLORS = {
  EMPTY: "empty",
  GRAY: "gray",
  YELLOW: "yellow",
  GREEN: "green",
};

function App() {
  const [grid, setGrid] = useState(
    Array(6)
      .fill()
      .map(() => Array(5).fill({ letter: "", color: COLORS.EMPTY })),
  );
  const [currentRow, setCurrentRow] = useState(0);
  const [currentCol, setCurrentCol] = useState(0);
  const [suggestions, setSuggestions] = useState([
    "WORDS",
    "WORLD",
    "WORTH",
    "WOULD",
    "WOUND",
  ]);
  const [errorMessage, setErrorMessage] = useState("");

  const cellRefs = useRef(
    Array(6)
      .fill()
      .map(() => Array(5).fill(null)),
  );
  const hiddenInputRef = useRef(null);

  useEffect(() => {
    if (hiddenInputRef.current) {
      hiddenInputRef.current.focus();
    }
  }, [currentRow, currentCol]);

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

  const generateSuggestions = () => {
    console.log("Generating suggestions based on constraints...");

    const constraints = grid.slice(0, currentRow + 1).map((row) =>
      row.map((cell) => ({
        letter: cell.letter.toLowerCase(),
        state: cell.color,
      })),
    );

    console.log("Constraints:", constraints);

    const mockSuggestions = [
      ["WORLD", "WORDS", "WORRY", "WORKS", "WORTH"],
      ["PRIZE", "PRIME", "PRINT", "PRIOR", "PRIDE"],
      ["TRACK", "TRAIN", "TRAIT", "TRAMP", "TRASH"],
      ["GHOST", "GLOOM", "GLORY", "GLOSS", "GLOVE"],
    ];

    const newSuggestions = mockSuggestions[currentRow - 1] || [
      "WORDS",
      "WORLD",
      "WORTH",
      "WOULD",
      "WOUND",
    ];
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
        setTimeout(() => setErrorMessage(""), 2000); // Clear message after 2 seconds
      } else if (!colorsComplete) {
        setErrorMessage("All cells must be colored before proceeding");
        setTimeout(() => setErrorMessage(""), 2000); // Clear message after 2 seconds
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

  const getCellStyle = (color, isActive, rowIndex) => {
    const isDisabled = rowIndex > currentRow;

    const baseStyle = {
      width: "3.5rem",
      height: "3.5rem",
      border: "2px solid #2c2c2c",
      borderRadius: "12px",
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      fontSize: "1.5rem",
      fontWeight: "bold",
      cursor: isDisabled ? "not-allowed" : "pointer",
      transition: "all 0.2s",
      userSelect: "none",
      opacity: isDisabled ? 0.5 : 1,
      transform: isActive ? "scale(1.05)" : "scale(1)",
      boxShadow: isActive
        ? "0 0 12px rgba(29, 155, 240, 0.5)"
        : "0 2px 5px rgba(0,0,0,0.2)",
    };

    switch (color) {
      case COLORS.GRAY:
        return {
          ...baseStyle,
          backgroundColor: "#3A3A3C",
          borderColor: "#3A3A3C",
          color: "white",
        };
      case COLORS.YELLOW:
        return {
          ...baseStyle,
          backgroundColor: "#B59F3B",
          borderColor: "#B59F3B",
          color: "white",
        };
      case COLORS.GREEN:
        return {
          ...baseStyle,
          backgroundColor: "#538D4E",
          borderColor: "#538D4E",
          color: "white",
        };
      default:
        return {
          ...baseStyle,
          backgroundColor: isActive ? "#1d1d1f" : "#121212",
          borderColor: isActive ? "#1D9BF0" : "#2c2c2c",
          color: "white",
        };
    }
  };

  return (
    <div className="min-h-screen bg-black text-white">
      <div className="flex flex-col items-center p-8 pr-80">
        <h1 className="text-4xl font-bold mb-6 text-center">Wordle Solver</h1>

        {errorMessage && (
          <div className="bg-red-900 border border-red-700 text-white px-4 py-3 rounded-xl mb-6 text-center max-w-md animate-pulse">
            {errorMessage}
          </div>
        )}

        {/* Hidden input for keyboard capture */}
        <input
          ref={hiddenInputRef}
          type="text"
          autoFocus
          autoComplete="off"
          onKeyDown={handleKeyDown}
          style={{
            position: "absolute",
            opacity: 0,
            pointerEvents: "none",
            height: 0,
            width: 0,
          }}
        />

        {/* Centered Grid */}
        <div className="mb-8 mt-4">
          <div
            style={{
              display: "flex",
              flexDirection: "column",
              gap: "0.5rem",
              alignItems: "center",
            }}
          >
            {grid.map((row, rowIndex) => (
              <div
                key={`row-${rowIndex}`}
                style={{ display: "flex", flexDirection: "row", gap: "0.5rem" }}
              >
                {row.map((cell, colIndex) => {
                  const isActive =
                    rowIndex === currentRow && colIndex === currentCol;
                  return (
                    <div
                      key={`cell-${rowIndex}-${colIndex}`}
                      ref={(el) => (cellRefs.current[rowIndex][colIndex] = el)}
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

        <div className="mb-8 max-w-md text-center">
          <p className="text-gray-300 mb-2">
            Click a cell and type to enter a word. Use ↑/↓ to change colors.
          </p>
          <p className="text-gray-300">
            Press Enter when your word is complete to get suggestions.
          </p>
        </div>
      </div>

      {/* Render our separate Sidebar component */}
      <Sidebar suggestions={suggestions} />
    </div>
  );
}

export default App;
