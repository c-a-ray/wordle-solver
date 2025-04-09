import React from "react";
import Sidebar from "./components/Sidebar";
import Grid from "./components/Grid";
import Instructions from "./components/Instructions";
import Loading from "./components/Loading";
import ResetButton from "./components/ResetButton";
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
    isLoading,
    cellRefs,
    hiddenInputRef,
    handleKeyDown,
    handleCellClick,
    resetGameState,
  } = useWordleGame();

  // Add error boundary to prevent blank screen
  try {
    return (
      <div className="app-container">
        <Sidebar suggestions={suggestions || []} />

        <div className="main-content">
          <h1 className="app-title">Wordle Solver</h1>

          <ResetButton onClick={resetGameState} disabled={isLoading} />

          {/* Hidden input for keyboard capture */}
          <input
            ref={hiddenInputRef}
            type="text"
            autoFocus
            autoComplete="off"
            onKeyDown={handleKeyDown}
            className="hidden-input"
            disabled={isLoading}
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
          <Instructions
            errorMessage={errorMessage}
            isSuccess={errorMessage.includes("success")}
          />

          {/* Loading Overlay */}
          {isLoading && <Loading />}
        </div>
      </div>
    );
  } catch (error) {
    console.error("Error rendering App:", error);
    return (
      <div
        style={{
          padding: "20px",
          color: "white",
          backgroundColor: "black",
          height: "100vh",
          display: "flex",
          flexDirection: "column",
          justifyContent: "center",
          alignItems: "center",
        }}
      >
        <h1>Wordle Solver</h1>
        <p>Something went wrong. Please check the console for details.</p>
        <button
          onClick={() => window.location.reload()}
          style={{
            marginTop: "20px",
            padding: "8px 16px",
            backgroundColor: "#1D9BF0",
            color: "white",
            border: "none",
            borderRadius: "24px",
            cursor: "pointer",
          }}
        >
          Refresh Page
        </button>
      </div>
    );
  }
}

export default App;
