import React from "react";
import Sidebar from "./components/Sidebar";
import Grid from "./components/Grid";
import Instructions from "./components/Instructions";
import useWordleGame from "./hooks/useWordleGame";
import "./styles/global.css";
import "./App.css";

/**
 * Main App component for the Wordle Solver application
 * @returns {JSX.Element} The rendered application
 */
function App() {
  const {
    grid,
    currentRow,
    currentCol,
    suggestions,
    errorMessage,
    cellRefs,
    hiddenInputRef,
    handleKeyDown,
    handleCellClick,
  } = useWordleGame();

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

        {/* Wordle Grid */}
        <Grid
          grid={grid}
          currentRow={currentRow}
          currentCol={currentCol}
          cellRefs={cellRefs}
          onCellClick={handleCellClick}
        />

        {/* Instructions and Error Messages */}
        <Instructions errorMessage={errorMessage} />
      </div>
    </div>
  );
}

export default App;
