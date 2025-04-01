import React from "react";
import WordleCell from "./WordleCell";

const WordleRow = ({
  row,
  rowIndex,
  currentRow,
  currentCol,
  onCellClick,
  onKeyDown,
  cellRefs,
}) => {
  return (
    <div className="flex gap-1.5">
      {row.map((cell, colIndex) => (
        <WordleCell
          key={`cell-${rowIndex}-${colIndex}`}
          letter={cell.letter}
          color={cell.color}
          isActive={rowIndex === currentRow && colIndex === currentCol}
          onClick={() => onCellClick(rowIndex, colIndex)}
          onKeyDown={(e) => onKeyDown(e, rowIndex, colIndex)}
          cellRef={(el) => {
            if (cellRefs.current) {
              cellRefs.current[rowIndex][colIndex] = el;
            }
          }}
        />
      ))}
    </div>
  );
};

export default WordleRow;
