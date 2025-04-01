import React from "react";

// A more complete but still simplified WordleGrid
const WordleGrid = () => {
  console.log("Rendering WordleGrid with proper layout");
  
  // Create a 6x5 grid structure for Wordle
  const gridRows = Array(6).fill().map(() => Array(5).fill({letter: "", color: "empty"}));
  
  return (
    <div className="mb-6">
      <div className="flex flex-col gap-2">
        {gridRows.map((row, rowIndex) => (
          <div key={`row-${rowIndex}`} className="flex gap-2">
            {row.map((cell, colIndex) => (
              <div
                key={`cell-${rowIndex}-${colIndex}`}
                className={`
                  w-14 h-14
                  border-2 border-gray-300
                  flex justify-center items-center
                  text-2xl font-bold
                  bg-white dark:bg-gray-800
                `}
              >
                {cell.letter}
              </div>
            ))}
          </div>
        ))}
      </div>
    </div>
  );
};

export default WordleGrid;
