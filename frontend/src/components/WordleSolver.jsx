import React from "react";
import WordleGrid from "./WordleGrid";
import Instructions from "./Instructions";
import Suggestions from "./Suggestions";
// Temporarily remove the hook
// import useGridManager from "../hooks/useGridManager";

const WordleSolver = () => {
  console.log("Rendering WordleSolver");
  
  // Remove hook usage for now
  // const {
  //   grid,
  //   currentRow,
  //   currentCol,
  //   suggestions: hookSuggestions,
  //   cellRefs,
  //   handleKeyDown,
  //   handleCellClick,
  // } = useGridManager();

  // Dummy data for testing
  const suggestions = ["HELLO", "WORLD", "TESTS"];

  return (
    <div className="flex flex-col items-center px-4 py-8 max-w-2xl mx-auto">
      <p className="text-xl mb-6">Simplified WordleSolver Component</p>

      {/* Simplified WordleGrid with no props */}
      <WordleGrid />

      <Instructions />

      <Suggestions suggestions={suggestions} />
    </div>
  );
};

export default WordleSolver;
