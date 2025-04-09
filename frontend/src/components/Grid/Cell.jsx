import React from "react";
import { getCellStyle } from "../../utils/colorUtils";

/**
 * Individual cell component for the Wordle grid
 * @param {Object} props - Component props
 * @param {string} props.letter - The letter to display in the cell
 * @param {string} props.color - The color state of the cell
 * @param {boolean} props.isActive - Whether the cell is currently active/focused
 * @param {number} props.rowIndex - The row index of the cell
 * @param {number} props.colIndex - The column index of the cell
 * @param {number} props.currentRow - The current active row
 * @param {function} props.onClick - Click handler function
 * @param {function} props.ref - Ref forwarded from parent
 * @returns {JSX.Element} Rendered cell component
 */
const Cell = React.forwardRef(
  (
    { letter, color, isActive, rowIndex, colIndex, currentRow, onClick },
    ref,
  ) => {
    const cellStyle = getCellStyle(color, isActive, rowIndex, currentRow);

    return (
      <div
        ref={ref}
        style={cellStyle}
        onClick={onClick}
        tabIndex={-1}
        data-row={rowIndex}
        data-col={colIndex}
      >
        {letter}
      </div>
    );
  },
);

Cell.displayName = "Cell";

export default Cell;
