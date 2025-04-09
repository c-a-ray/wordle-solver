import { COLORS } from "./constants";

/**
 * Get the next color when cycling up through colors
 * @param {string} currentColor - The current color
 * @returns {string} - The next color in the cycle
 */
export const getNextColorUp = (currentColor) => {
  switch (currentColor) {
    case COLORS.EMPTY:
      return COLORS.GRAY;
    case COLORS.GRAY:
      return COLORS.YELLOW;
    case COLORS.YELLOW:
      return COLORS.GREEN;
    case COLORS.GREEN:
      return COLORS.EMPTY;
    default:
      return COLORS.EMPTY;
  }
};

/**
 * Get the next color when cycling down through colors
 * @param {string} currentColor - The current color
 * @returns {string} - The next color in the cycle
 */
export const getNextColorDown = (currentColor) => {
  switch (currentColor) {
    case COLORS.EMPTY:
      return COLORS.GREEN;
    case COLORS.GREEN:
      return COLORS.YELLOW;
    case COLORS.YELLOW:
      return COLORS.GRAY;
    case COLORS.GRAY:
      return COLORS.EMPTY;
    default:
      return COLORS.EMPTY;
  }
};

/**
 * Generate styles for a cell based on its color state, active state, and row index
 * @param {string} color - The color state of the cell
 * @param {boolean} isActive - Whether the cell is currently active/focused
 * @param {number} rowIndex - The row index of the cell
 * @param {number} currentRow - The current active row
 * @returns {Object} - The style object to apply to the cell
 */
export const getCellStyle = (color, isActive, rowIndex, currentRow) => {
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
