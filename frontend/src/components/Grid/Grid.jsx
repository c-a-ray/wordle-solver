import React from "react";
import Cell from "./Cell";
import "./Grid.css";

/**
 * Grid component for the Wordle puzzle
 * @param {Object} props - Component props
 * @param {Array<Array<Object>>} props.grid - 2D array of cell states
 * @param {number} props.currentRow - Current active row
 * @param {number} props.currentCol - Current active column
 * @param {React.RefObject} props.cellRefs - Refs for all cells
 * @param {function} props.onCellClick - Cell click handler
 * @returns {JSX.Element} Rendered grid component
 */
const Grid = ({ grid, currentRow, currentCol, cellRefs, onCellClick }) => {
  return (
    <div className="grid-container">
      <div className="grid">
        {grid.map((row, rowIndex) => (
          <div key={`row-${rowIndex}`} className="grid-row">
            {row.map((cell, colIndex) => {
              const isActive =
                rowIndex === currentRow && colIndex === currentCol;
              return (
                <Cell
                  key={`cell-${rowIndex}-${colIndex}`}
                  ref={(el) => (cellRefs.current[rowIndex][colIndex] = el)}
                  letter={cell.letter}
                  color={cell.color}
                  isActive={isActive}
                  rowIndex={rowIndex}
                  colIndex={colIndex}
                  currentRow={currentRow}
                  onClick={() => onCellClick(rowIndex, colIndex)}
                />
              );
            })}
          </div>
        ))}
      </div>
    </div>
  );
};

export default Grid;
