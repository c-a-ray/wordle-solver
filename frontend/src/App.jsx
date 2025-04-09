import React, { useState, useRef, useEffect } from "react";
import Sidebar from "./components/Sidebar";
import "./App.css";

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
    <div className="app-container">
      <Sidebar suggestions={suggestions} />

      <div className="main-content">
        <h1 className="app-title">Wordle Solver</h1>
        {/* Hidden input for keyboard capture */}
        <input
          ref={hiddenInputRef}
          type="text"
          autoFocus
          autoComplete="off"
          onKeyDown={handleKeyDown}
          className="hidden-input"
        />
        {/* Centered Grid */}
        <div className="grid-container">
          <div className="grid">
            {grid.map((row, rowIndex) => (
              <div key={`row-${rowIndex}`} className="grid-row">
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
        <div className="instructions-container">
          <p className="instruction-text mb-2">
            Click a cell and type to enter a word. Use ↑/↓ to change colors.
          </p>
          <p className="instruction-text">
            Press Enter when your word is complete to get suggestions.
          </p>
          {errorMessage && <div className="error-message">{errorMessage}</div>}
        </div>
      </div>
    </div>
  );
}

export default App;
